function get_id(clicked_id) {
     Shiny.setInputValue("current_id", clicked_id, {priority: "event"});
}

function resetExampleLocation() {
  document.getElementById('example_panel').style.inset = 'unset';
  document.getElementById('example_panel').style.top = '60px';
  document.getElementById('example_panel').style.left = '440px';
  document.getElementById('example_panel').style.bottom = 'auto';
  document.getElementById('example_panel').style.right = 'auto';
}

function resetDemoLocation() {
  document.getElementById('demo_panel').style.inset = 'unset';
  document.getElementById('demo_panel').style.top = '60px';
  document.getElementById('demo_panel').style.left = '440px';
  document.getElementById('demo_panel').style.bottom = 'auto';
  document.getElementById('demo_panel').style.right = 'auto';
}

function resetHelpLocation() {
  document.getElementById('help_panel').style.inset = 'unset';
  document.getElementById('help_panel').style.top = '60px';
  document.getElementById('help_panel').style.left = '440px';
  document.getElementById('help_panel').style.bottom = 'auto';
  document.getElementById('help_panel').style.right = 'auto';
}