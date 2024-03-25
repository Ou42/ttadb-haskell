// JS funcs called when buttons clicked
const deleteToDo = async b => {
  const r = await fetch(`/${b.value}`, {
            method: 'DELETE'
  })
  document.querySelector(`#todo-${b.value}`).remove()
};

