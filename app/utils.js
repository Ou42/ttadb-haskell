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

/*
const updateToDo = b => {
    fetch(`/edit/${b.value}`, {
        method: 'GET'
    })
}

// .then( console.log('Hello!'); ); }"
*/