//JavaScript para mostrar mensajes emegentes en pantalla en la APP de shiny.

Shiny.addCustomMessageHandler("testmessage",
  function(message) {
    alert(JSON.stringify(message));
  }
);