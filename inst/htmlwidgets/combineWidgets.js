HTMLWidgets.widget({

  name: 'combineWidgets',

  type: 'output',

  factory: function(el, width, height) {

    var widgets = [];

    function toArray(x) {
      if (x.constructor !== Array) x = [x];
      return x;
    }

    function getWidgetFactory(name) {
      return HTMLWidgets.widgets.filter(function(x) {return x.name == name})[0];
    }

    function resizeAll() {
      widgets.forEach(function(x) {
        x.factory.resize(x.el, x.el.clientWidth, x.el.clientHeight, x.instance);
      });
    }

    return {

      renderValue: function(x) {
        x.elementId = toArray(x.elementId);
        x.widgetType = toArray(x.widgetType);

        var nWidgets = x.widgetType.length;
        el.innerHTML = x.html;

        for (var i = 0; i < nWidgets; i++) {
          var child = document.getElementById(x.elementId[i]);

          if (x.widgetType[i] == "html") {
            child.innerHTML = x.data[i];
          } else {
            var widgetFactory = getWidgetFactory(x.widgetType[i]);
            var w = widgetFactory.initialize(child, child.clientWidth, child.clientHeight);
            widgetFactory.renderValue(child, x.data[i], w);
            widgets.push({factory:widgetFactory, instance:w, el: child});
          }
        }

        if (HTMLWidgets.shinyMode) {
          // I don't why, but is necessary when launching a shiny gadget in the Rstudio viewer
          setTimeout(resizeAll, 1);
        }

      },

      resize: function(width, height) {
        resizeAll();
      }

    };
  }
});
