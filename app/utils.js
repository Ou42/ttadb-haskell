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

function edit(id) {
    console.log(`edit(${id})`);
    console.log(`edit id = ${id.value}`);
    console.log(id);
    document.querySelector(".update_id").value = id.value;
    document.querySelector(".update_form").style.display = "block";
}

function update() {
    console.log("update()");
    var id = document.querySelector(".update_id").value;
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