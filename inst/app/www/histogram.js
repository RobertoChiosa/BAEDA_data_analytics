// !preview r2d3 data=c(0.3, 0.6, 0.8, 0.95, 0.40, 0.20)

// X axis: scale and draw:
var x = d3.scaleLinear()
    .domain([0, 1])  
      .range([0, width]);
  svg.append("g")
      .attr("transform", "translate(0," + height + ")")
      .call(d3.axisBottom(x));

// set the parameters for the histogram
var histogram = d3.histogram()
    .value(function(d) { return d.price; })   // I need to give the vector of value
    .domain(x.domain())  // then the domain of the graphic
    .thresholds(x.ticks(70)); // then the numbers of bins

  // And apply this function to data to get the bins
var bins = histogram(data);

  // Y axis: scale and draw:
var y = d3.scaleLinear()
    .range([height, 0]);
    y.domain([0, d3.max(bins, function(d) { return d.length; })]);   // d3.hist has to be called before the Y axis obviously
svg.append("g")
    .call(d3.axisLeft(y));

  // append the bar rectangles to the svg element
svg.selectAll("rect")
    .data(bins)
    .enter()
    .append("rect")
      .attr("x", 1)
      .attr("transform", function(d) { return "translate(" + x(d.x0) + "," + y(d.length) + ")"; })
      .attr("width", function(d) { return x(d.x1) - x(d.x0) -1 ; })
      .attr("height", function(d) { return height - y(d.length); })
      .style("fill", "#69b3a2")