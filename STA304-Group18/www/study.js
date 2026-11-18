document.addEventListener("change", (event) => {
  const input = event.target;
  if (input.matches("input[data-group-colour]")) {
    Shiny.setInputValue(input.id, input.value);
  }
});
