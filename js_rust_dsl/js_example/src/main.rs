use js_runtime::JsFunction;
use js_macro::js;
use js_runtime::JsValue; 

fn main() {
    println!("--- Running JS-like script ---");
    js! {
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

        console.log("--- Array example ---");
        let my_array = [1, "two", true];
        console.log("Initial array:", my_array);

        my_array.push(4);
        console.log("After push(4):", my_array);

        let popped_value = my_array.pop();
        console.log("Popped value:", popped_value);
        console.log("After pop():", my_array);

        console.log("--- Object example ---");
        _const person = {
            firstName: "John",
            lastName: "Doe",
             age: 30,
            greet: function(x) {
                console.log(x);
            }
        };
        person.greet("Hello!");
        console.log(person.firstName + " " + person.lastName + " is " + person.age + " years old.");


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