use uuid::Uuid;
use juniper::FieldResult;

type PipelineId = Uuid;

#[derive(juniper::GraphQLInputObject)]
struct CompilationOptions {
    latitude: f64, //TODO
}

pub struct Query;

#[derive(juniper::GraphQLObject)]
struct Span {
    start: i32,
    end: i32,
}
 
#[derive(juniper::GraphQLObject)]
struct OptimizationMappingToListing {
    id: Uuid,
    name: String,
    span: Span,
}

//TODO: llvm_ir, diff_view 

#[juniper::object]
impl Query {
    fn optimization_layout(&self) -> FieldResult<Vec<OptimizationMappingToListing>> {
        println!("test");
        Ok(vec![
            OptimizationMappingToListing{id: Uuid::new_v4(), name: "test1".to_string(), span: Span{start: 1, end: 2}},
            OptimizationMappingToListing{id: Uuid::new_v4(), name: "test1".to_string(), span: Span{start: 3, end: 4}},
            OptimizationMappingToListing{id: Uuid::new_v4(), name: "test1".to_string(), span: Span{start: 5, end: 6}},
        ])
    }
    // Return immutable graph that hold all change produced by the compiler 
    fn ast_graph() -> String {
        todo!();
        //next_state //prev_state
    }
    fn log() -> String {
        todo!();
    }   
}

pub struct Mutations;

#[juniper::object]
impl Mutations {
    fn create_compilation_pipeline(listing: String) -> String { //TODO options: ResearchDescriptor -> FieldResult<PipelineId>
        println!("{}", listing);
        todo!();
    }
}

pub type Schema = juniper::RootNode<'static, Query, Mutations>;

pub fn create_schema() -> Schema {
    Schema::new(Query, Mutations)
}