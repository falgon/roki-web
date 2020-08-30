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


function newton_raphson(f, fd, init_val) {
    'use strict';
    const fn = x => x - f(x) / fd(x);
    let res = [];
    let xn = null;
    for (let x0 = init_val, i = 0;; x0 = xn, ++i) {
        res.push(x0);
        xn = fn(x0);
        if (Math.abs(xn - x0) < Number.EPSILON || i > 99) break;
    }
    return res;
}

const Graph = (() => {
    'use strict';
    const start = 0, length = 500, dens = 0.01;
    const init_axis = Symbol('init_axis'),
        tangent_f = Symbol('tangent_f'),
        make_dataset = Symbol('make_dataset'),
        update_newton = Symbol('update_newton'),
        input = Symbol('input'),
        update_scale = Symbol('update_scale'),
        update_axis = Symbol('update_axis'),
        add_new_f = Symbol('add_new_f');
    const visited = 'vis',
        input_f = 'func',
        input_f_diff = 'func_differential',
        input_init = 'initial_value',
        width = 400,
        height = 300,
        fline_delay = 1500,
        tanline_delay = 400,
        margin = Object.freeze({ "top": 30, "bottom": 60, "right": 30, "left": 60 });
    const is_undefined = x => { return typeof x === "undefined"; }

    class Graph {
        constructor(f, fd, init_val) {
            if (is_undefined(f) || is_undefined(fd) || is_undefined(init_val)) {
                this.x_scale = undefined;
                this.y_scale = undefined;
                this.axisx = undefined;
                this.axisy = undefined;
                this.svg = undefined;
                this.message = 'Invalid parameter.';
                this.is_ok = false;
                this.nr = undefined;
            } else {
                this.nr = [];
                this.x_scale = d3.scaleLinear()
                    .domain([0, 10])
                    .range([margin.left, width - margin.right]);
                this.y_scale = d3.scaleLinear()
                    .domain([0, 10])
                    .range([height - margin.bottom, margin.top]);
                this.axisx = d3.axisBottom(this.x_scale).ticks(5);
                this.axisy = d3.axisLeft(this.y_scale).ticks(5);
                this.svg = d3.select("#" + visited).append("svg")
                    .attr("width", width)
                    .attr("height", height);
 
                this[init_axis]();
 
                this.is_ok = true;
            }
        }

        [init_axis]() {
            this.svg.append("g")
                .attr("class", "x axis")
                .attr("transform", "translate(" + 0 + "," + (height - margin.bottom) + ")")
                .call(this.axisx)
                .append("text")
                .attr("fill", "black")
                .attr("x", (width - margin.left - margin.right) / 2 + margin.left)
                .attr("y", 35)
                .attr("text-anchor", "middle")
                .attr("font-size", "10pt")
                .attr("font-weight", "bold")
                .text("x");
 
            this.svg.append("g")
                .attr("class", "y axis")
                .attr("transform", "translate(" + margin.left + "," + 0 + ")")
                .call(this.axisy)
                .append("text")
                .attr("fill", "black")
                .attr("text-anchor", "middle")
                .attr("x", -(height - margin.top - margin.bottom) / 2 - margin.top)
                .attr("y", -35)
                .attr("transform", "rotate(-90)")
                .attr("font-weight", "bold")
                .attr("font-size", "10pt")
                .text("y");
        }

        [make_dataset](f, st, size) {
            let b = start, l = length;
            if (!is_undefined(st)) b = st;
            if (!is_undefined(size)) l = size;
            
            const res = Array.apply(null, new Array(l)).map((v, i) => [b + i * dens , f(b + i * dens)]);
            if (res.map(Number.isNaN).reduce((acc, n) => acc || n)) {
                return undefined;
            }
            return res;
        }

        write_error(s) {
            if (document.getElementById("success_panel").style.display === "block") {
                document.getElementById("success_panel").style.display = "none";
            }
            document.getElementById("error_message").innerHTML = s;
            document.getElementById("error_panel").style.display = "block";
        }

        [tangent_f](f, fd, e, x) {
            return fd(e) * x - fd(e) * e + f(e);
        }

        [update_newton](nr, f, fd, init_val) {
            d3.selectAll(".lin").remove();
            document
                .getElementById("success_panel")
                .style
                .display = "block";
            document
                .getElementById("success_message")
                .innerHTML = "The result is: <img src=\"https://latex.codecogs.com/svg.latex?{0}\"/>".format(nr[nr.length - 1]);
            setTimeout(() => {
                nr.forEach((e, i) => {
                    const lin = Array.apply(null, new Array(length))
                        .map((v, i) => [start + i * dens, this[tangent_f](f, fd, e, start + i * dens)])
                        .reverse();
                    this.svg.append("path")
                        .datum(lin)
                        .attr("class", "lin " + i)
                        .attr("fill", "none")
                        .attr("stroke", "#82B446")
                        .attr("stroke-width", 1.5)
                        .attr("d", d3.line().x(d => this.x_scale(d[0])).y(d => this.y_scale(d[1])))
                        .attr('stroke-dasharray', '385 385')
                        .attr('stroke-dashoffset', 385)
                        .transition()
                        .delay(tanline_delay + tanline_delay * i)
                        .attr('stroke-dashoffset', 0);
                });
            }, fline_delay);
        }

        [input]() {
            if (document.getElementById("success_panel").style.display == "block") {
                document.getElementById("success_panel").style.display = "none";
            }

            const fs = document.getElementById(input_f).value;
            if (fs == null || fs == "") {
                this.write_error('The function \\(f(x)\\) must be defined.');
                return undefined;
            }

            const fds = document.getElementById(input_f_diff).value;
            if (fds == null || fds == "") {
                this.write_error('The derivative \\(f\'(x)\\) of \\(f(x)\\) must be defined.');
                return undefined;
            }

            const inits = document.getElementById(input_init).value;
            if (inits == null || inits == "") {
                this.write_error('The initial value must be defined.');
                return undefined;
            }

            return Object.freeze({fs: fs, fds: fds, inits: inits});
        }

        [update_scale](dataset) {
            this.x_scale.domain([d3.min(dataset, d => d[0]), d3.max(dataset, d => d[0])]);
            this.y_scale.domain([d3.min(dataset, d => d[1]), d3.max(dataset, d => d[1])]);
        }

        [update_axis]() {
            this.svg.select(".x.axis")
                .transition()
                .duration(750)
                .call(this.axisx);

            this.svg.select(".y.axis")
                .transition()
                .duration(750)
                .call(this.axisy);
        }

        [add_new_f](dataset) {
            this.svg.select(".line").remove();
            this.svg.append("path")
                .datum(dataset)
                .attr("class", "line")
                .attr("fill", "none")
                .attr("stroke", "steelblue")
                .attr("stroke-width", 3)
                .attr("d", d3.line().x(d => this.x_scale(d[0])).y(d => this.y_scale(d[1])))
                .attr("stroke-dasharray", "385 385")
                .attr("stroke-dashoffset", 385)
                .transition()
                .duration(fline_delay)
                .attr('stroke-dashoffset', 0);
        }

        update() {
            if (!this.is_ok) return;
            const it = this[input]();
            if (is_undefined(it)) return;

            const regexp = /\([^)]*\)/g;
            try {
                var f = math.parse('f(x) = ' + it.fs).compile();
                var f_diff = math.parse('g(x) = ' + it.fds).compile();
                var init_val = math.parse(it.inits).compile();
                var nr = newton_raphson(f.evaluate(), f_diff.evaluate(), init_val.evaluate());
                var dataset = this[make_dataset](f.evaluate(), nr[nr.length - 1]);
            } catch (e) {
                this.write_error(e.toString().replace(regexp, ""));
                return;
            }
            if (is_undefined(dataset)) {
                this.write_error('Divided by zero');
                return;
            }
            if (document.getElementById("error_panel").style.display == "block") {
                document.getElementById("error_panel").style.display = "none";
            }
            
            this[update_scale](dataset);
            this[add_new_f](dataset);
            this[update_axis]();

            try {
                this[update_newton](nr, f.evaluate(), f_diff.evaluate(), init_val.evaluate());
            } catch (e) {
                this.write_error(e.toString().replace(regexp, ""));
                return;
            }
        }
    }

    return Graph;
})();

var g_graph = undefined;
    
const main = (() => {
    g_graph = new Graph(x => x * x - 2, x => x * 2, 5);

    if (!g_graph.is_ok) {
        write_error(g_graph.message + "\nPlease reloading.");
    }
})();

function update() {
    g_graph.update();
}
