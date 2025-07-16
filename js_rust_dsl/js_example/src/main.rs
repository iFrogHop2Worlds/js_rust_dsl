use js_macro::js_script;
use js_runtime::JsValue; 

fn main() {
    println!("--- Running JS-like script ---");
    js_script! {
        function greet(name) {
            console.log("Hello, " + name);
        }

        greet("World");

        console.log("--- For loop ---");
        for (let i = 0; i < 5; i = i + 1) {
            console.log(i);
        }

        console.log("--- While loop ---");
        let j = 0;
        while (j < 3) {
            greet(j);
            j = j + 1;
        }

        console.log("--- Do-while loop ---");
        let k = 0;
        do {
            console.log("k = " + k);
            k = k + 1;
        } while (k < 2);

    }
    println!("--- Script finished ---");

    // You can also directly interact with JsValue
    let js_num: JsValue = 5.0.into();
    let js_str: JsValue = "Rust".into();
    let result = js_num.add(&js_str);
    println!("Direct JsValue addition (5 + \"Rust\"): {}", result); 

    let js_bool_true: JsValue = true.into();
    let js_bool_false: JsValue = false.into();
    let js_null: JsValue = JsValue::Null;
    let js_undefined: JsValue = JsValue::Undefined;
    let js_zero: JsValue = 0.0.into();
    let js_empty_str: JsValue = "".to_string().into();

    println!("Truthiness of true: {}", js_bool_true.to_bool()); 
    println!("Truthiness of false: {}", js_bool_false.to_bool()); 
    println!("Truthiness of null: {}", js_null.to_bool()); 
    println!("Truthiness of undefined: {}", js_undefined.to_bool()); 
    println!("Truthiness of 0: {}", js_zero.to_bool());
    println!("Truthiness of empty string: {}", js_empty_str.to_bool()); 
    println!("Truthiness of 10: {}", JsValue::Number(10.0).to_bool()); 
    println!("Truthiness of 'hello': {}", JsValue::String("hello".to_string()).to_bool());

}