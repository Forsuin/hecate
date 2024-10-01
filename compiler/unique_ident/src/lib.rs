static mut VAR_COUNTER: i32 = 0;
static mut LABEL_COUNTER: i32 = 0;

pub fn make_temp() -> String {
    unsafe {
        let string = format!("tmp.{}", VAR_COUNTER);
        VAR_COUNTER += 1;
        string
    }
}

pub fn make_label(prefix: &str) -> String {
    unsafe {
        let string = format!("{}.{}", prefix, LABEL_COUNTER);
        LABEL_COUNTER += 1;
        string
    }
}

pub fn make_temp_name(prefix: &str) -> String {
    make_label(prefix)
}
