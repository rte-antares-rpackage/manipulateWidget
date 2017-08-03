$( document ).ready(function() {
    $(".mw-btn-settings,.mw-btn-area")
	  .click(select)
	  .each(function(i) {
		$(this).data("index", i);
	  });
});

function select(e) {
  var el = $(e.currentTarget);
  var active = el.hasClass("active");
  $(".mw-btn-settings,.mw-btn-area").removeClass("active");
  $(".mw-inputs").css("display", "none");
  if (!active) {
	el.addClass("active");
	var i = el.data("index");
	$(".mw-inputs").eq(i).css("display", "block");
  }

  // Resize all widgets
  var widgets = HTMLWidgets.findAll(document, ".mw-chart>.html-widget");
  var ids = $.map($(".mw-chart>.html-widget"), function(x, i) {return x.id});
  var container;
  if (widgets) {
	  for (var i = 0; i < widgets.length; i++) {
		  container = document.getElementById(ids[i]);
		  HTMLWidgets.widgets[0].resize(container, container.clientWidth, container.clientHeight, widgets[i]);
	  }
  }
}


