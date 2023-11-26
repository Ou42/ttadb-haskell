// JS funcs called when buttons clicked
const deleteToDo = b => {
    fetch(`/${b.value}`, {
            method: 'DELETE'
        })
        .then(r => b.parentElement.remove());
};

const toggleVisUpdateForm = b => {
    console.log(`/edit/${b.value}`);
    const x = document.getElementById(`editform${b.value}`);
    if (x.style.display === 'none') {
        x.style.display = 'block';
    } else {
        x.style.display = 'none';
    }
};

const updateToDo = b => {
    window.location.href = `/edit/${b.value}`;
};


// ---------------------------------------------

// YT: Code w/ Yousaf - Creating a CRUD ...

// Q-01: function vs const syntax?!
// Q-02: document.querySelector vs getElementById?!
// Q-03: if searching for a class name, is "." always req?
// Q-04: How to make this less brittle?
//          - Some use `.value`, some `.text`, & others `.innerText`
// Q-05: .replace() doesn't require sending in regex in quotes?!

function edit(btn) {
    const id = btn.value;

    // document.querySelector(".update_id").value = id;
    // document.getElementById("update_id").value = id;
    document.querySelector("[name='update_id']").value = id;
    document.querySelector(".update_form").style.display = "block";

    const todoStr = `[name='todo: ${id}']`;

    const todoItemText = document.querySelector(todoStr).text;

    const prev_todo = document.querySelector("[name='prev_todo']");
    prev_todo.innerText = "Current: " + todoItemText;

    document.querySelector("[name='updated_todo']").value = todoItemText;

    // multi-line template literal, removing all dbl-space
    console.log(`edit(${btn}) ---
    | edit id = ${id}
    | todoStr = ${todoStr}
    | todoItemText = ${todoItemText}`.replace(/  +/g, ''));
    console.log(prev_todo);
    console.log(btn);
}

function update() {
    const id = document.querySelector("[name='update_id']").value;

    const updated_todo = document.querySelector("[name='updated_todo']").value;

    document.querySelector(".update_form").style.display = "none";

    // multi-line template literal, removing all dbl-space
    console.log(`update() ---
    | id = ${id}
    | updated_todo = ${updated_todo}`.replace(/  +/g, ''));
}

/*
const updateToDo = b => {
    fetch(`/edit/${b.value}`, {
        method: 'GET'
    })
}

// .then( console.log('Hello!'); ); }"
*/