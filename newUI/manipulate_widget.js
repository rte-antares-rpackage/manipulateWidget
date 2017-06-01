$(".mw-btn-settings,.mw-btn-area")
  .click(select)
  .each(function(i) {
    $(this).data("index", i);
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
}
