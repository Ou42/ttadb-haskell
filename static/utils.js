// JS funcs called when buttons clicked
const deleteToDo = async b => {
  const r = await fetch(`/${b.value}`, {
            method: 'DELETE'
  })
  document.querySelector(`#todo-${b.value}`).remove()
};

// ---------------------------------------------

/*
Q-01: function vs const syntax?!
Q-02: document.querySelector vs getElementById?!
Q-03: if searching for a class name, is "." always req?
Q-04: How to make this less brittle?
         - Some use `.value`, some `.text`, & others `.innerText`
Q-05: .replace() doesn't require sending in regex in quotes?!
Q-06: changed HTML.html to HTML.docTypeHtml, any reason for HTML.html?
Q-07: return a fetch()?
Q-08: .then(response => response.json());
      ... Uncaught (in promise) SyntaxError: JSON.parse:
                unexpected end of data at line 1 column 1 of the JSON data
Q-09: PUT vs PATCH?!
      ... Scotty *is* the server?
      ... So, I'm responsible for "imitating" PUT / PATCH functionality?!
      <https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods/PUT>
      <https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods/PATCH>
Q-10: remove fav-icon not found! A: .link ! Attr.rel & .href
*/

