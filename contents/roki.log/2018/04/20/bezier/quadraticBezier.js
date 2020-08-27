/*
 * Copyright (C) 2019- Roki. All rights reserved.
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), 
 * to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, 
 * and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, 
 * WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 *
 */

function dispBezier() {
    'use strict';
    const width = 700, height = 400;
    const svg = d3.select("#vis").append("svg").attr("width", width).attr("height", height);

    function bezier(t, p0, p1, p2) {
        return (1 - t) * ((1 - t) * p0 + t * p1) + t * ((1 - t) * p1 + t * p2);
    }

    function bezierLine(t, p0, p1) {
        return (1 - t) * p0 + t * p1;
    }

    function roundT(tt) {
        return Math.round(t * 1000) / 1000;
    }

    let x0 = 100, y0 = 350;
    let x1 = 250, y1 = 50;
    let x2 = 400, y2 = 350;
    let t = 0, last = 0;
    let xs = [x0, x1, x2], ys = [y0, y1, y2];

    const radius    = 10;
    const LPcolor   = "gray"

    const lineAB    = svg.append("line").attr("x1", x0).attr("y1", y0).attr("x2", x1).attr("y2", y1).attr("stroke", LPcolor).attr("stroke-width", "1px");
    const lineBC    = svg.append("line").attr("x1", x1).attr("y1", y1).attr("x2", x2).attr("y2", y2).attr("stroke", LPcolor).attr("stroke-width", "1px");

    const bCurve    = svg.append("path").attr("d", "M " + x0 + "," + y0 + " Q 250,50 400,350").attr("stroke", "red").attr("stroke-width", "2px").attr("fill", "none");
    const P0        = svg.append("text").attr("x", x0).attr("y", y0).attr("dx", "-25").text("P0");
    const P1        = svg.append("text").attr("x", x1).attr("y", y1).attr("dx", "-23").text("P1");
    const P2        = svg.append("text").attr("x", x2).attr("y", y2).attr("dx", "25").text("P2");
    const B         = svg.append("text").attr("x", x0).attr("y", y0).attr("dx", "-25").text("B");
    const Q0        = svg.append("text").attr("x", x0).attr("y", y0).attr("dx", "-25").text("Q0");
    const Q1        = svg.append("text").attr("x", x1).attr("y", y1).attr("dx", "25").text("Q1");
    const T         = svg.append("text").attr("x", 10).attr("y", 10).text("t: " + roundT(t));

    const movingLine    = svg.append("line").attr("x1", x0).attr("y1", y0).attr("x2", x1).attr("y2", y1).attr("stroke", "green").attr("stroke-width", "1px");
    const movingCircle  = svg.append("circle").attr("cx", x0).attr("cy", y0).attr("r", "3px").attr("stroke", "black").attr("stroke-width", "1px").attr("fill", "black");
    const movingCircle1 = svg.append("circle").attr("cx", x0).attr("cy", y0).attr("r", "3px").attr("stroke", "black").attr("stroke-width", "1px").attr("fill", "none");
    const movingCircle2 = svg.append("circle").attr("cx", x1).attr("cy", y1).attr("r", "3px").attr("stroke", "black").attr("stroke-width", "1px").attr("fill", "none");

    function dragcall(event, d) {
        event.sourceEvent.stopPropagation();
    }

    function radmax(ev) {
        return [ Math.max(radius, Math.min(width - radius, ev.x)), Math.max(radius, Math.min(height - radius, ev.y)) ];
    }

    function dragmove0(event) {
        [x0, y0] = radmax(event);

        d3.select(this).attr("cx", x0).attr("cy", y0);
        bCurve.attr("d", "M " + x0 + "," + y0 + " Q "+ x1 + "," + y1 + " " + x2 + "," + y2);
        P0.attr("x", x0).attr("y", y0);
        lineAB.attr("x1", x0).attr("y1", y0);
        movingCircle.attr("cx", x0).attr("cy", y0);
        B.attr("x", x0).attr("y", y0);
        movingCircle1.attr("cx", x0).attr("cy", y0);
        Q0.attr("x", x0).attr("y", y0);
    }

    function dragmove1(event) {
        [x1, y1] = radmax(event);

        d3.select(this).attr("cx", x1).attr("cy", y1);
        bCurve.attr("d", "M "+ x0 + "," + y0 + " Q "+ x1 + "," + y1 + " " + x2 + "," + y2);
        P1.attr("x", x1).attr("y", y1);
        lineAB.attr("x2", x1).attr("y2", y1);
        lineBC.attr("x1", x1).attr("y1", y1);
        movingCircle2.attr("cx", x1).attr("cy", y1);
        Q1.attr("x", x1).attr("y", y1);
    }

    function dragmove2(event) {
        [x2, y2] = radmax(event);
    
        d3.select(this).attr("cx", x2).attr("cy", y2);
        bCurve.attr("d", "M " + x0 + "," + y0 + " Q " + x1 + "," + y1 + " " + x2 + "," + y2);
        P2.attr("x", x2).attr("y", y2);
        lineBC.attr("x2", x2).attr("y2", y2);
        movingCircle2.attr("cx", x2).attr("cy", y2);
        Q1.attr("x", x2).attr("y", y2);
    }

    let dgs = [], dgsf = [dragmove0, dragmove1, dragmove2];
    [...Array(3)].map(() => dgs.push(d3.drag().on("start", dragcall)));
    dgs.map((val, i) => {
        svg.append("circle").attr("cx", xs[i]).attr("cy", ys[i]).attr("r", radius).attr("stroke", "black").attr("stroke-width", "1px").attr("fill", LPcolor).call(val.on("drag", dgsf[i]));
    });
    
    d3.timer((elapsed) => {
        t = (t + (elapsed - last) / 5000) % 1;
        last = elapsed;

        const x     = Math.round(bezier(t, x0, x1, x2)), y = Math.round(bezier(t, y0, y1, y2));
        const lx1   = Math.round(bezierLine(t, x0, x1)), lx2 = Math.round(bezierLine(t, x1, x2)), ly1 = Math.round(bezierLine(t, y0, y1)), ly2 = Math.round(bezierLine(t, y1, y2));

        movingLine.attr("x1", lx1).attr("y1", ly1).attr("x2", lx2).attr("y2", ly2).attr("stroke", "green").attr("stroke-width", "1px");
        movingCircle.attr("cx", x).attr("cy", y).attr("r", "3px").attr("stroke", "black").attr("stroke-width", "1px").attr("fill", "black");
        B.attr("x", x).attr("y", y);

        movingCircle1.attr("cx", lx1).attr("cy", ly1).attr("r", "3px").attr("stroke", "black").attr("stroke-width", "1px").attr("fill", "none");
        Q0.attr("x", lx1).attr("y", ly1);
        movingCircle2.attr("cx", lx2).attr("cy", ly2).attr("r", "3px").attr("stroke", "black").attr("stroke-width", "1px").attr("fill", "none");  
        Q1.attr("x", lx2).attr("y", ly2);
        T.attr("width", 10).attr("height", 10).text("t: " + roundT(t));
    });

}

let on = false;
const drawBezier = (() => {
    var executed = false;
    return () => {
        if (!on) {
            on = true;
            document.getElementById("bezierButton").value = 'やめる'
            document.getElementById("vis").style.display = "block";
        } else {
            on = false;
            document.getElementById("bezierButton").value = '動かす'
            document.getElementById("vis").style.display = "none";
        }
        if (!executed) {
            executed = true;
            dispBezier();
        }
    };
})();
