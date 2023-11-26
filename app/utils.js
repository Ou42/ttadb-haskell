// JS funcs called when buttons clicked
const deleteToDo = b => {
    fetch(`/${b.value}`, {
            method: 'DELETE'
        })
        .then(r => b.parentElement.remove());
};

const toggleVisUpdateForm = b => {
    console.log(`/edit/${b.value}`);
    var x = document.getElementById(`editform${b.value}`);
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

function edit(btn) {
    var id = btn.value;
    console.log(`edit(${btn})`);
    console.log(btn);
    console.log(`edit id = ${id}`);

    // document.querySelector(".update_id").value = id.value;
    // document.getElementById("update_id").value = id.value;
    document.querySelector("[name='update_id']").value = id.value;
    document.querySelector(".update_form").style.display = "block";

    var todoStr = `[name='todo: ${id}']`;
    console.log(`todoStr = ${todoStr}`);

    var todoItemText = document.querySelector(todoStr).text;
    console.log(`todoItemText = ${todoItemText}`);

    var prev_todo = document.querySelector("[name='prev_todo']");
    prev_todo.innerText = "Current: " + todoItemText;
    console.log(prev_todo);

    document.querySelector("[name='updated_todo']").value = todoItemText;
}

function update() {
    console.log("update()");
    // var id = document.querySelector(".update_id").value;
    var id = document.querySelector("[name='update_id']").value;
    console.log(id);

    document.querySelector(".update_form").style.display = "none";
}

/*
const updateToDo = b => {
    fetch(`/edit/${b.value}`, {
        method: 'GET'
    })
}

// .then( console.log('Hello!'); ); }"
*/