// Task 1
function Singelton(arg) {
    return Array.of(arg)
}

// Task 2
function Null(list) {
    if (Array.isArray(list)) {
        return Length(list) ? false : true
    }
    return "Not a list"
}

// Task 3
function Snoc(list, arg) {
    if (Array.isArray(list)){    
        list.push(arg);    
        return list
    }
    return "Not a list"
}

// Task 4
function Length(list) {
    if (Array.isArray(list)){ 
        let count = 0;
        for (el of list){
            count++;
        }
        return count;
    }
    return "Not a list"
}


// Result 1
const arg1 = "Hello World"
const res1 = Singelton(arg1)
console.log(`Result 1: ${res1}`)

// Result 2
const list2full = ["Hello", "World"]
const res2full = Null(list2full)
console.log(`Result 2 (full): ${res2full}`)
const list2empty = []
const res2empty = Null(list2empty)
console.log(`Result 2 (empty): ${res2empty}`)

// Result 3
const list3 = ["Hello", "World"]
const arg3 = "!"
const res3 = Snoc(list3, arg3)
console.log(`Result 3: ${res3}`)

// Result 4
const list4 = ["Hello", "World", "!"]
const res4 = Length(list4)
console.log(`Result 4 (full): ${res4}`)
const res4Empty = Length([])
console.log(`Result 4 (empty): ${res4Empty}`)