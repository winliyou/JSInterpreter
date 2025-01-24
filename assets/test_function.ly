function myfun() {
    print("from function");
}

myfun();

function myfun2(v1) {
    return v1 + 123;
}

let b = myfun2(100);
print("b is ", b);