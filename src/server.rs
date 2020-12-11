use std::net::TcpListener;
use std::thread::spawn;
use std::net::ToSocketAddrs;
use tungstenite::server::accept;
use juniper::{
    Variables,
    http::GraphQLRequest
};
use serde_json;

use crate::schema::create_schema;

pub struct WsServer {
    server: TcpListener,
}

impl WsServer {
    pub fn new<T: ToSocketAddrs>(address: T) -> Self {
        WsServer{server: TcpListener::bind(address).unwrap()}
    }
    pub fn idle(self) {
        let query = r#"
            mutation {
                createCompilationPipeline(listing: "test")
            }
        "#;
        let query2 = r#"
            query {
                optimizationLayout {
                    id
                    name
                    span {
                        start,
                        end
                    }
                }
            }
        "#;

        let schema = create_schema();

        let (res, _errors) = juniper::execute(
            query2,
            None,
            &schema,
            &Variables::new(),
            &(),
        ).unwrap();

        println!("{}", serde_json::to_string(&res).unwrap());
       
        // for stream in self.server.incoming() {
        //     spawn (move || {
        //         let mut websocket = accept(stream.unwrap()).unwrap();
        //         loop {
        //             let msg = websocket.read_message().unwrap();
                    
        //             if msg.is_binary() || msg.is_text() {
        //                 println!("{}", msg);
        //                 let req = GraphQLRequest::new("test".to_string(), None, None);
        //                 let resp = req.execute(&create_schema(), &());
        //             }
        //         }
        //     });
        // }
    }
}



