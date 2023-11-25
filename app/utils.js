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

// function vs const syntax?!

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
    // var todoLI = document.querySelector(todoStr).value;
    var todoLI = document.querySelector(todoStr);
    console.log(`todo = ${todoLI}`);
    console.log(todoLI);

    // document.getElementById("prev_todo").value = "hmm ??? hmm";
    // document.getElementById("updated_todo").value = "??? hmmm ???";
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