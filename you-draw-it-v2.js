
// you-draw-it.js
// Modes: "plain" | "record" | "highlight"

r2d3.onRender(function (data, svg, width, height, options) {
  svg.selectAll("*").remove();

  const plots = data.plots;
  const mode  = (options && options.mode) || "plain";
  const SHOW_TOOLS = (mode === "highlight");

  // ---------- layout ----------
  const plotWidth  = 200;
  const plotHeight = 150;

  const topShelfH  = 20;
  const headerH    = 22;
  const gap        = 2;
  const hGap = 12, vGap = 4;

  const cellHeight = topShelfH + gap + headerH + gap + plotHeight;
  const nCols = 5, nRows = 4;

  const svgW  = nCols * (plotWidth + hGap) - hGap;
  const svgH  = nRows * (cellHeight + vGap) - vGap;

  svg.attr("viewBox", `0 0 ${svgW} ${svgH}`)
     .attr("preserveAspectRatio", "xMidYMid meet")
     .attr("width", "100%")
     .attr("height", "100%");

  const defs = svg.append("defs");
  const f = defs.append("filter").attr("id", "btnShadow").attr("height", "130%");
  f.append("feDropShadow")
    .attr("dx", 0).attr("dy", 1).attr("stdDeviation", 2)
    .attr("flood-color", "#000").attr("flood-opacity", 0.25);

  const container = svg.append("g");

  // ---------- selection bookkeeping ----------
  const MAX_SEL = 2;
  let selectedSet = new Set();
  let selectionOrder = [];
  const cells = [];

  function applySelectionStyle(idx, on) {
    const c = cells[idx];
    if (!c) return;
    c.bgRect.attr("stroke", on ? "red" : "#ccc").attr("stroke-width", on ? 3 : 1);
    const showControls = SHOW_TOOLS && on;
    c.topShelfG.style("display", showControls ? null : "none");
    c.overlay.style("display", showControls ? null : "none").style("pointer-events", "none");
  }

  function pointInPolygon(px, py, poly) {
    let inside = false;
    for (let a = 0, b = poly.length - 1; a < poly.length; b = a++) {
      const [xi, yi] = poly[a], [xj, yj] = poly[b];
      const intersect = ((yi > py) !== (yj > py)) &&
                        (px < (xj - xi) * (py - yi) / ((yj - yi) || 1e-9) + xi);
      if (intersect) inside = !inside;
    }
    return inside;
  }

  function simplify(points, tol = 1.5) {
    if (points.length <= 2) return points;
    const res = [points[0]];
    for (let k = 1; k < points.length - 1; k++) {
      const [x0, y0] = res[res.length - 1];
      const [x1, y1] = points[k];
      if (Math.hypot(x1 - x0, y1 - y0) >= tol) res.push(points[k]);
    }
    res.push(points[points.length - 1]);
    return res;
  }

  // ---------- build grid ----------
  plots.forEach((plotData, i) => {
    const col = i % nCols, row = Math.floor(i / nCols);

    const cellG = container.append("g")
      .attr("id", `cell-${i}`)
      .attr("transform", `translate(${col * (plotWidth + hGap)}, ${row * (cellHeight + vGap)})`);

    // top shelf (hidden unless selected)
    const topShelfG = cellG.append("g")
      .attr("class", "top-shelf")
      .attr("transform", `translate(0, 0)`)
      .style("display", "none");

    const btnH = topShelfH, btnGap = 6;

    const highlightBtn = topShelfG.append("g").attr("class", "btn-highlight").style("cursor", "pointer");
    const hRect = highlightBtn.append("rect")
      .attr("width", 97).attr("height", btnH).attr("rx", 6)
      .attr("fill", "#FFD54F").attr("stroke", "#946200").attr("stroke-width", 1.5)
      .attr("filter", "url(#btnShadow)");
    highlightBtn.append("text")
      .attr("x", 97/2).attr("y", btnH/2 + 0.5)
      .attr("text-anchor", "middle").attr("dominant-baseline", "middle")
      .attr("font-size", 12).attr("font-weight", "700").text("Highlight");
    highlightBtn.on("mouseenter", () => hRect.attr("fill", "#FFC107"))
                .on("mouseleave", () => hRect.attr("fill", "#FFD54F"));

    const clearBtn = topShelfG.append("g").attr("class", "btn-undo")
      .attr("transform", `translate(${97 + btnGap}, 0)`).style("cursor", "pointer");
    const uRect = clearBtn.append("rect")
      .attr("width", 97).attr("height", btnH).attr("rx", 6)
      .attr("fill", "#ECEFF1").attr("stroke", "#607D8B").attr("stroke-width", 1.5)
      .attr("filter", "url(#btnShadow)");
    clearBtn.append("text")
      .attr("x", 97/2).attr("y", btnH/2 + 0.5)
      .attr("text-anchor", "middle").attr("dominant-baseline", "middle")
      .attr("font-size", 12).attr("font-weight", "700").text("Undo");
    clearBtn.on("mouseenter", () => uRect.attr("fill", "#CFD8DC"))
            .on("mouseleave", () => uRect.attr("fill", "#ECEFF1"));

    // number bar
    const numberY = topShelfH + gap;
    const numBar = cellG.append("g").attr("transform", `translate(0, ${numberY})`);
    numBar.append("rect").attr("width", plotWidth).attr("height", headerH).attr("fill", "#d9d9d9");
    numBar.append("text")
      .attr("x", plotWidth / 2).attr("y", headerH / 2)
      .attr("text-anchor", "middle").attr("dominant-baseline", "middle")
      .attr("font-weight", "bold").text(i + 1);

    // plot area
    const plotY = topShelfH + gap + headerH + gap;
    const plotG = cellG.append("g").attr("class", "plot")
      .attr("transform", `translate(0, ${plotY})`);

    // margins inside plot area for axes
    const leftPad = 40, rightPad = 16, bottomPad = 26, topPad = 18;

    const x = d3.scaleLinear().domain([0, 15]).range([leftPad, plotWidth - rightPad]);
    const yExtent = d3.extent(plotData.y);
    const y = d3.scaleLinear()
      .domain([yExtent[0]-1, yExtent[1]+1])
      .range([plotHeight - bottomPad, topPad]);

    const bgRect = plotG.append("rect")
      .attr("width", plotWidth).attr("height", plotHeight)
      .attr("fill", "#fff").attr("stroke", "#ccc").attr("stroke-width", 1)
      .style("cursor", SHOW_TOOLS ? "pointer" : "default")
      .style("pointer-events", SHOW_TOOLS ? "all" : "none");

    // --- axes with explicit tick label styling (restores numbers) ---
    const xAxis = d3.axisBottom(x).ticks(4);
    const yAxis = d3.axisLeft(y).ticks(4);

    const gx = plotG.append("g")
      .attr("transform", `translate(0, ${plotHeight - bottomPad})`)
      .call(xAxis);
    gx.selectAll("text").attr("font-size", 10).attr("fill", "#555");
    gx.selectAll("path,line").attr("stroke", "#888");

    const gy = plotG.append("g")
      .attr("transform", `translate(${leftPad}, 0)`)
      .call(yAxis);
    gy.selectAll("text").attr("font-size", 10).attr("fill", "#555");
    gy.selectAll("path,line").attr("stroke", "#888");

    // points
    const points = plotData.x.map((d, j) => ({ x: d, y: plotData.y[j] }));
    plotG.selectAll("circle").data(points).enter().append("circle")
      .attr("cx", d => x(d.x)).attr("cy", d => y(d.y)).attr("r", 2.5).attr("fill", "black");

    // drawing layer + overlay
    const drawLayer = plotG.append("g").attr("class", "draw-layer");
    const overlay = plotG.append("rect")
      .attr("width", plotWidth).attr("height", plotHeight)
      .attr("fill", "transparent")
      .style("display", "none")
      .style("pointer-events", "none");

    let drawing = false, path = null, screenPts = [], drawStartMs = null;

    overlay.on("mousedown", function(event){
        if (!SHOW_TOOLS) return;
        drawing = true; screenPts = []; drawStartMs = Date.now();
        path = drawLayer.append("path")
          .attr("fill","rgba(255,165,0,0.12)")
          .attr("stroke","orange").attr("stroke-width",2)
          .attr("vector-effect","non-scaling-stroke");
        const [x0, y0] = d3.pointer(event, this);
        screenPts.push([x0,y0]);
        path.attr("d", `M${x0},${y0}`);
      })
      .on("mousemove", function(event){
        if(!drawing) return;
        const [x1,y1] = d3.pointer(event, this);
        screenPts.push([x1,y1]);
        path.attr("d", `M${screenPts[0][0]},${screenPts[0][1]}L${screenPts.slice(1).map(p=>p.join(",")).join(" ")}`);
      })
      .on("mouseup mouseleave", function(){
        if(!drawing) return;
        drawing = false;
        const drawEndMs = Date.now();

        screenPts = simplify(screenPts);
        if (screenPts.length < 3) { if (path) path.remove(); return; }

        const closed = [...screenPts, screenPts[0]];
        path.attr("d", `M${closed[0][0]},${closed[0][1]}L${closed.slice(1).map(p=>p.join(",")).join(" ")}Z`);

        const polygonData = screenPts.map(([sx,sy]) => [x.invert(sx), y.invert(sy)]);
        const selected = [];
        points.forEach((pt, idx) => {
          if (pointInPolygon(pt.x, pt.y, polygonData)) selected.push({ index: idx, x: pt.x, y: pt.y });
        });

        if (HTMLWidgets.shinyMode) {
          Shiny.setInputValue("highlighted_region", {
            plotIndex: i + 1,
            selectedPoints: selected,
            drawStartedAt: drawStartMs,
            drawEndedAt: drawEndMs,
            durationMs: drawEndMs - drawStartMs
          }, { priority: "event" });
        }
      });

    // toggle selection
    bgRect.on("click", () => {
      if (!SHOW_TOOLS) return;

      const wasSelected = selectedSet.has(i);
      let nowSelected;

      if (wasSelected) {
        selectedSet.delete(i);
        selectionOrder = selectionOrder.filter(k => k !== i);
        applySelectionStyle(i, false);
        nowSelected = false;
      } else {
        if (selectedSet.size >= MAX_SEL) {
          const drop = selectionOrder.shift();
          selectedSet.delete(drop);
          applySelectionStyle(drop, false);
        }
        selectedSet.add(i);
        selectionOrder.push(i);
        applySelectionStyle(i, true);
        nowSelected = true;
      }

      if (HTMLWidgets.shinyMode) {
        Shiny.setInputValue("plot_clicked", {
          plotIndex: i + 1,
          selected:  nowSelected,
          action:    nowSelected ? "select" : "deselect"
        }, { priority: "event" });
      }
    });

    // tool buttons
    highlightBtn.on("click", () => {
      if (!SHOW_TOOLS) return;
      cells.forEach((c) => c.overlay.style("pointer-events", "none"));
      overlay.style("display", null).style("pointer-events", "all");
    });

    clearBtn.on("click", () => {
      if (!SHOW_TOOLS) return;
      const paths = drawLayer.selectAll("path").nodes();
      if (paths.length) d3.select(paths[paths.length - 1]).remove();
      if (HTMLWidgets.shinyMode) {
        Shiny.setInputValue("highlight_cleared", { plotIndex: i + 1 }, { priority: "event" });
      }
    });

    cells[i] = { topShelfG, bgRect, overlay };
  });
});
