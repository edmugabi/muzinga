use std::collections::BTreeMap;
use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashSet;
use std::fmt;

use std::collections::HashMap;
use std::collections::VecDeque;

use lexer::Lexer;
use parser::Parser;
use evaluator::Expr;
use evaluator::Value;

#[derive(Clone,Debug,PartialEq)]
pub struct Sheet(Rc<RefCell<BTreeMap<NodeRef,Node>>>);

unsafe impl Send for Sheet {}
//unsafe impl Sync for Sheet {}

impl Sheet {
    pub fn new() -> Self {
        Sheet(Rc::new(RefCell::new(BTreeMap::new())))
    }

    pub fn get_val(&self, nr: &NodeRef) -> Value {
        let ref map = *(self.0.borrow());
        let ref node = map.get(nr).unwrap();
            node.get_val()
    }

    pub fn update<F>(&self, update_ref: &NodeRef, update_string: &str, set_changed: &F)
            where F:  Fn(&NodeRef,String)
    {
        let mut l = Lexer::new(update_string);
        let tokens = l.lex();
        let mut p = Parser::new(&tokens);
        let (expr,reads) = p.parse().unwrap();
        let new_node = Node::new(expr.clone());

        let node: &Node = {
            let mut map = self.0.borrow_mut();
            &map.entry(update_ref.clone())
                                     .or_insert(new_node).clone() // a node address
        };

        let reads: HashSet<_> = reads.into_iter().collect();

        //TODO: this is also done even for the newly inserted node: need to optimize
        //THE expr stores also a copy of the reads, could eliminate the read graph that way
        {
            let old_reads = node.read.borrow();
            {
                let old_to_update: HashSet<_> = old_reads.difference(&reads).collect();
                for nr in &old_to_update {
                    self.0.borrow().get(nr).unwrap().remove_observer(update_ref);
                }
            }
            {
                // do map instead of collecting first
                let new_to_update: HashSet<_> = reads.difference(&old_reads).collect();
                for nr in &new_to_update {
                    self.0.borrow().get(nr).unwrap().update_observer(update_ref.clone());
                }
            }
        }

        node.set_expr(expr);
        node.update_reads(reads);
        node.eval(&self, set_changed);  // or self.eval(&node,set_changed);
        set_changed(update_ref,node.get_val().to_string());  //update happens after observer nodes
    }


    pub fn add_nodes(&self, nodes: &mut VecDeque<(NodeRef,String)>) -> Rc<RefCell<HashMap<NodeRef,String>>> {
        let ns = Rc::new(RefCell::new(HashMap::new()));
        while let Some((nr,s)) = nodes.pop_front() {

            let ns_clone = ns.clone();
            self.update(&nr,&s, &|nr: &NodeRef, s: String| {
                (*ns.borrow_mut()).insert(nr.clone(),s);
            } );
        }
        ns
    }
}

#[derive(Clone,PartialEq,PartialOrd,Eq,Ord,Hash)]
pub struct NodeRef {
    row: i32,
    col: String,
}

//TODO: Make it error safe
impl<'a> From<&'a str> for NodeRef {
    fn from(s: &'a str) -> Self {
        let (col,row) = s.split_at(1);
        NodeRef {
            row: row.parse::<i32>().unwrap(),
            col: col.to_owned(),
        }
    }
}

impl fmt::Debug for NodeRef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}{}", self.col, self.row)
    }
}

impl fmt::Display for NodeRef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}{}", self.col,self.row)
    }
}

impl NodeRef {
    pub fn new(row: i32, col: String) -> Self {
        NodeRef {
            row: row,
            col: col,
        }
    }
}
#[derive(Clone,PartialEq)]
struct Node {
    val: Rc<RefCell<Option<Value>>>,
    expr: Rc<RefCell<Expr>>,
    observers: Rc<RefCell<HashSet<NodeRef>>>,
    read: Rc<RefCell<HashSet<NodeRef>>>,
}

impl fmt::Debug for Node {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("Node")
            .field("val",&*self.val.borrow())
            .field("expr",&*self.expr.borrow())
            .field("observ",&*self.observers.borrow())
            .field("read",&*self.read.borrow())
            .finish()
    }
}

impl Node {
    fn new(expr: Expr) -> Self {
        Node {
            val: Rc::new(RefCell::new(None)),
            expr: Rc::new(RefCell::new(expr)),
            observers: Rc::new(RefCell::new(HashSet::new())),
            read: Rc::new(RefCell::new(HashSet::new())),
        }
    }

    fn get_val(&self) -> Value {
        self.val.borrow().clone().unwrap()
    }

    fn set_expr(&self,expr: Expr) {
        (*self.expr.borrow_mut()) = expr;
    }

    fn eval<F>(&self, sheet: &Sheet, set_changed: &F)
            where F: Fn(&NodeRef,String) {
        let ref expr = *self.expr.borrow();
        let new_val = Some(expr.eval(sheet));
        (*self.val.borrow_mut()) = new_val;

        let ref observers = *self.observers.borrow();
        for a_node_ref in observers {
            let map = sheet.0.borrow();
            let a_node = map.get(a_node_ref).unwrap();
            a_node.eval(sheet, set_changed);
            set_changed(a_node_ref, a_node.get_val().to_string());
        }
    }

    fn update_reads(&self, new_reads: HashSet<NodeRef>) {
        *self.read.borrow_mut() = new_reads;
        // let union: HashSet<_> = self.read.borrow().union(&new_reads).cloned().collect();
        // *self.read.borrow_mut() = union;
    }

    fn update_observer(&self, new_observer: NodeRef) {
        self.observers.borrow_mut().insert(new_observer);
    }

    fn remove_observer(&self, nr: &NodeRef) {
        self.observers.borrow_mut().remove(nr);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_sheet_1() {
        let sheet = Sheet::new();

        let check_results_1 = |nr: &NodeRef,s: String| {
            assert_eq!("A1".to_owned(), nr.to_string());
            assert_eq!("2".to_owned(), s);
        };
        sheet.update(&NodeRef::from("A1"), "1 + 1", &check_results_1 );

        let check_results_2 = |nr: &NodeRef,s: String| {
            let nr: &str = &nr.to_string();
                match nr {
                    "A2" => assert_eq!("4".to_owned(), s),
                    _ => {}
                };

        };
        sheet.update(&NodeRef::from("A2"), "2 + A1", &check_results_2 );

        let check_results_3 = |nr: &NodeRef,s: String| {
            let nr: &str = &nr.to_string();
            assert!(["A2","A1"].contains(&nr));
                match nr {
                    "A2" => assert_eq!("6".to_owned(), s),
                    "A1" => assert_eq!("4".to_owned(), s),
                    _ => {}
                };

        };
        sheet.update(&NodeRef::from("A1"), "4", &check_results_3 );
    }

    #[test]
    fn test_sample_spreadsheet() {
        let cells = vec![
                        ("C1","200"),
                        ("C2","300"),
                        ("C3","700"),
                        ("C4","250"),
                        ("D1","2"),
                        ("D2","3"),
                        ("D3","1"),
                        ("D4","4"),
                        ("E1","C1*D1"),
                        ("E2","C2*D2"),
                        ("E3","C3*D3"),
                        ("E4","C4*D4"),
                        ("E5","E1+E2+E3+E4"),
                        ("E6","E5/4")
                    ];

        let mut input: VecDeque<(NodeRef,String)> = cells.into_iter()
                                    .map( |(nr,s)| { (NodeRef::from(nr), s.to_string()) })
                                    .collect();
        let list = vec![
                        ("E6","750"),
                        ("E5","3000"),
                        ("E4","1000"),
                        ("E3","700"),
                        ("E2","900"),
                        ("E1","400"),
                        ("D4","4"),
                        ("D3","1"),
                        ("D2","3"),
                        ("D1","2"),
                        ("C4","250"),
                        ("C3","700"),
                        ("C2","300"),
                        ("C1","200"),
                    ];
        let hm: HashMap<NodeRef,String> = list.into_iter()
                                            .map(|(nr,s)| { (NodeRef::from(nr), s.to_string()) })
                                            .collect();

        let expected_changed_cells = Rc::new(RefCell::new(hm));

            let sheet = Sheet::new();
            let updated_observers = sheet.add_nodes(&mut input);
            assert_eq!(expected_changed_cells, updated_observers);

            // update two quantities and verify that the results propagate
            //D1 = 5, D3 = 2
            let cell_update = vec![
                    ("D1","5"),
                    ("D3","2"),
            ];
            let mut new_input: VecDeque<(NodeRef,String)> = cell_update.into_iter()
                                        .map( |(nr,s)| { (NodeRef::from(nr), s.to_string()) })
                                        .collect();

            let new_list = vec![
                    ("D1","5"),
                    ("D3","2"),
                    ("E1","1000"),
                    ("E3","1400"),
                    ("E5","4300"),
                    ("E6","1075")
            ];

            let hm: HashMap<NodeRef,String> = new_list.into_iter()
                                                .map(|(nr,s)| { (NodeRef::from(nr), s.to_string()) })
                                                .collect();

            let expected_changed_cells = Rc::new(RefCell::new(hm));

            let updated_observers = sheet.add_nodes(&mut new_input);
            assert_eq!(expected_changed_cells, updated_observers,"{:#?}",sheet);
            println!("{:#?}",sheet);
            //remove the fourth row from the sum and see whether the changes propagate.
            let cell_update = vec![
                    ("E5","E1+E2+E3"),
                    ("D4","2") //E4 should update, but the sum shd not
            ];
            let mut new_input: VecDeque<(NodeRef,String)> = cell_update.into_iter()
                                        .map( |(nr,s)| { (NodeRef::from(nr), s.to_string()) })
                                        .collect();
            let new_list = vec![
                    ("D4","2"),
                    ("E4","500"), // E4 should update but E5 dependency has been removed.
                    ("E5","3300"),
                    ("E6","825")
            ];

            let hm: HashMap<NodeRef,String> = new_list.into_iter()
                                                .map(|(nr,s)| { (NodeRef::from(nr), s.to_string()) })
                                                .collect();

            let expected_changed_cells = Rc::new(RefCell::new(hm));

            let updated_observers = sheet.add_nodes(&mut new_input);
            assert_eq!(expected_changed_cells, updated_observers,"{:#?}",sheet);

    }
}
