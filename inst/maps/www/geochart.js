google.load('visualization', '1', {'packages': ['corechart', 'geochart']});
google.setOnLoadCallback(function() {
  window.isGoogleLoaded = true;
  $(document).trigger('googleLoaded');
});

function dataFrameToDataTable(data) {
  var columns = Object.keys(data);
  if (columns.length == 0)
    return [];
  var dt = [columns];
  for (var i = 0; i < data[columns[0]].length; i++) {
    var row = [];
    for (var j = 0; j < columns.length; j++) {
      row.push(data[columns[j]][i]);
    }
    dt.push(row);
  }
  return google.visualization.arrayToDataTable(dt);
}

function waitForGoogleLoad(func) {
  if (window.isGoogleLoaded) {
    setTimeout(func, 1);
  } else {
    $(document).one('googleLoaded', func);
  }
}

function constructGoogleChart(el, name, dataObj, Chart) {
  if (dataObj == null)
    return;
  
  var data = dataFrameToDataTable(dataObj.data);

  var $el = $(el);
  var chart = $el.data('geochart');
  if (!chart) {
    chart = new Chart(el);
    $el.data('geochart', chart);
    chart.options = JSON.parse($el.attr('data-options'));
    google.visualization.events.addListener(chart, 'select', function() {
      var value = null;
      var selection = chart.getSelection();
      if (selection && selection.length > 0) {
        value = chart.data.getValue(selection[0].row, 0);
      }
      var update = {};
      update[name + '_selection'] = value;
      Shiny.shinyapp.sendInput(update);
    });
  }
  
  var currentOptions = $.extend(true, chart.options, dataObj.options);
  chart.draw(data, currentOptions);
  chart.data = data;
}

var GeochartOutputBinding = new Shiny.OutputBinding();
$.extend(GeochartOutputBinding, {
  find: function(scope) {
    return $(scope).find('.shiny-geochart-output');
  },
  renderValue: function(el, dataObj) {
    var self = this;
    waitForGoogleLoad(function() {
      constructGoogleChart(el, self.getId(el), dataObj, google.visualization.GeoChart);
    });
  }
});
Shiny.outputBindings.register(GeochartOutputBinding, 'geochart');

var GoogleLineChartOutputBinding = new Shiny.OutputBinding();
$.extend(GoogleLineChartOutputBinding, {
  find: function(scope) {
    return $(scope).find('.google-linechart-output');
  },
  renderValue: function(el, dataObj) {
    var self = this;
    waitForGoogleLoad(function() {
      constructGoogleChart(el, self.getId(el), dataObj, google.visualization.LineChart);
    });
  }

});
Shiny.outputBindings.register(GoogleLineChartOutputBinding, 'google-linechart');
