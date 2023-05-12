$(document).on("shiny:sessioninitialized", function(event) {
  tooltip_toggle = function() {
    var elements = document.body.querySelectorAll( "[data-title]" );
    for ( var i = 0; i < elements.length; i++ ) {
        var tooltip = document.createElement( "div" );
        tooltip.setAttribute( "class", "tooltip" );
        tooltip.innerText = elements[ i ].getAttribute( "data-title" );
        elements[ i ].appendChild( tooltip );
        elements[ i ].addEventListener( "mouseenter", function( m_event ) {
            var tool_text = $("div.tooltip").text()
            if (tool_text != 'no-image-icon-23494.png') {
              $("div.tooltip").css("display","inline")
              $("div.tooltip").wrap('<a href="..." />');
            } else {
              tooltip.style.display = "none";
              $("div.tooltip").css("display","none")
            }
        } );
        elements[ i ].addEventListener( "mouseleave", function( m_event ) {
            $("div.tooltip").css("display","none")
        } );
    }
  }
} );
