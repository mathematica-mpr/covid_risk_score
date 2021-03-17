$(document).ready(function() {
  
  Shiny.addCustomMessageHandler('update_label', function(msg) {
    
    var selector = 'label[for=' + msg.id + ']';
    $(selector).html(msg.html);
    
  })
  
})