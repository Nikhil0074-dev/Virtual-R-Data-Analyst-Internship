const fs = require("fs");
const path = require("path");
const {
  Document, Packer, Paragraph, TextRun, ImageRun, Table, TableRow, TableCell,
  HeadingLevel, AlignmentType, LevelFormat, BorderStyle, WidthType, ShadingType,
  Footer, PageNumber, PageBreak,
} = require("docx");

const ROOT = path.resolve(__dirname, "..");
const FIG = (n) => path.join(ROOT, "figures", n);
const FONT = "Arial";
const MONO = "Courier New";
const W = 9360; // content width in DXA (US Letter, 1 inch margins)

// ---------- helpers ----------
function runs(text, base = {}) {
  // supports **bold** inside text
  return text.split("**").map((t, i) =>
    new TextRun({ text: t, bold: i % 2 === 1 ? true : base.bold, font: FONT, size: base.size || 22, color: base.color, italics: base.italics })
  );
}
const P = (text, o = {}) => new Paragraph({
  children: runs(text, o), spacing: { after: o.after ?? 120, line: 300 },
  alignment: o.align, keepNext: o.keepNext,
});
const H1 = (t) => new Paragraph({ heading: HeadingLevel.HEADING_1, children: [new TextRun({ text: t, font: FONT })] });
const H2 = (t) => new Paragraph({ heading: HeadingLevel.HEADING_2, children: [new TextRun({ text: t, font: FONT })] });
const H3 = (t) => new Paragraph({ heading: HeadingLevel.HEADING_3, children: [new TextRun({ text: t, font: FONT })] });
const bullets = (arr) => arr.map((t) => new Paragraph({
  numbering: { reference: "bul", level: 0 }, children: runs(t), spacing: { after: 80, line: 290 },
}));
const callout = (label, text) => new Paragraph({
  children: [new TextRun({ text: label + " ", bold: true, font: FONT, size: 22, color: "0B4F8A" }), ...runs(text)],
  shading: { type: ShadingType.CLEAR, fill: "E8F1FA", color: "auto" },
  border: { left: { style: BorderStyle.SINGLE, size: 24, color: "0072B2", space: 8 } },
  indent: { left: 200, right: 100 }, spacing: { before: 120, after: 200, line: 300 },
});
const label = (t) => new Paragraph({
  children: [new TextRun({ text: t, bold: true, font: FONT, size: 22, color: "333333" })],
  spacing: { before: 160, after: 60 }, keepNext: true,
});
function codeBlock(text) {
  const lines = text.replace(/\s+$/, "").split("\n");
  return lines.map((ln, i) => new Paragraph({
    children: [new TextRun({ text: ln === "" ? " " : ln, font: MONO, size: 16 })],
    shading: { type: ShadingType.CLEAR, fill: "F3F4F6", color: "auto" },
    spacing: { before: 0, after: 0, line: 240 },
    indent: { left: 120, right: 120 },
    keepLines: true,
    border: i === 0 ? { top: { style: BorderStyle.SINGLE, size: 4, color: "D0D5DB", space: 4 } }
      : i === lines.length - 1 ? { bottom: { style: BorderStyle.SINGLE, size: 4, color: "D0D5DB", space: 4 } } : undefined,
  })).concat([new Paragraph({ children: [], spacing: { after: 120 } })]);
}
function pngSize(file) {
  const b = fs.readFileSync(file);
  return { w: b.readUInt32BE(16), h: b.readUInt32BE(20) };
}
function image(file, caption, alt) {
  const { w, h } = pngSize(FIG(file));
  const width = 610, height = Math.round((h / w) * width);
  return [
    new Paragraph({
      alignment: AlignmentType.CENTER, keepNext: true, spacing: { before: 120, after: 60 },
      children: [new ImageRun({ type: "png", data: fs.readFileSync(FIG(file)),
        transformation: { width, height }, altText: { title: caption, description: alt, name: file } })],
    }),
    new Paragraph({ alignment: AlignmentType.CENTER, spacing: { after: 200 },
      children: [new TextRun({ text: caption, italics: true, font: FONT, size: 18, color: "555555" })] }),
  ];
}
const border = { style: BorderStyle.SINGLE, size: 4, color: "BFC5CC" };
const borders = { top: border, bottom: border, left: border, right: border };
function table(headers, rows, widths) {
  const mk = (t, wd, head) => new TableCell({
    width: { size: wd, type: WidthType.DXA }, borders,
    shading: head ? { type: ShadingType.CLEAR, fill: "0072B2", color: "auto" } : undefined,
    margins: { top: 60, bottom: 60, left: 100, right: 100 },
    children: [new Paragraph({ children: [new TextRun({ text: String(t), font: FONT, size: 19, bold: head, color: head ? "FFFFFF" : undefined })] })],
  });
  return [new Table({
    width: { size: W, type: WidthType.DXA }, columnWidths: widths,
    rows: [new TableRow({ tableHeader: true, children: headers.map((h, i) => mk(h, widths[i], true)) }),
      ...rows.map((r) => new TableRow({ cantSplit: true, children: r.map((c, i) => mk(c, widths[i], false)) }))],
  }), new Paragraph({ children: [], spacing: { after: 160 } })];
}

// ---------- pull code sections out of the real script ----------
const script = fs.readFileSync(path.join(ROOT, "scripts", "01_visualizations.R"), "utf8");
const parts = script.split(/^# ---- /m);
// parts[0] header, parts[1] = "0. Setup", parts[2] = "1. Load", parts[3..14] = figures 1..12, parts[15] = numbers
const section = (i) => ("# ---- " + parts[i]).replace(/\n# =+\s*$/m, "").trimEnd();
const figCode = (n) => section(n + 2);
const log = fs.readFileSync(path.join(ROOT, "output", "console_log.txt"), "utf8");
const logHead = log.split("`geom_smooth()`")[0].trimEnd();

// ---------- document content ----------
const c = [];

// Title block
c.push(new Paragraph({ spacing: { before: 1200, after: 120 }, children: [new TextRun({ text: "Week 2 Task", font: FONT, size: 26, color: "0072B2", bold: true })] }));
c.push(new Paragraph({ heading: HeadingLevel.TITLE, children: [new TextRun({ text: "Data Visualization and Insight Communication using R", font: FONT })] }));
c.push(P("Case study: How long do people live, and what does money have to do with it? (Gapminder data, 1952 to 2007)", { size: 26, color: "444444", after: 300 }));
c.push(...table(["Item", "Detail"], [
  ["Student", "[Your name]"],
  ["Course / Week", "[Course name], Week 2"],
  ["Tools", "R 4.3, ggplot2, dplyr, tidyr, scales, ggrepel, lattice, base R graphics"],
  ["Dataset", "Gapminder: 142 countries, 12 time points, 1,704 rows"],
  ["GitHub repository", "[Paste your repository link here]"],
], [2400, 6960]));

c.push(new Paragraph({ children: [new PageBreak()] }));

// 1 Executive summary
c.push(H1("1. Summary for busy readers"));
c.push(P("This report uses 12 charts to answer one question: **how has life expectancy changed around the world since 1952, and how much of that is linked to income?** Every chart was made with R code that is shown in this document and in the GitHub repository."));
c.push(label("The five main findings"));
c.push(...bullets([
  "**The world lives much longer.** Average life expectancy of countries rose from 49.1 to 67.0 years. Countries with life expectancy of 70 or more went from 5 to 83 (out of 142).",
  "**Richer usually means longer-lived.** Across countries in 2007, every 10 times increase in income goes with about 16.6 extra years of life. But the link is not perfect: income explains about 65% of the differences, not all of them.",
  "**Asia caught up fast. Africa did not.** Asia gained 26.5 years (population-weighted). Africa gained 15.8 years, and almost all of that came before 1987. From 1987 to 2002 African life expectancy rose by only about half a year.",
  "**Averages hide shocks.** Rwanda lost 20.4 years of life expectancy in one 5-year step (1987 to 1992 reading). In 20 countries, people lived shorter lives in 2007 than in 1992, and 17 of those are in Africa.",
  "**Continent labels hide large differences.** Inside Africa and Asia, the gap between the best and worst country is about 37 to 39 years.",
]));
c.push(callout("Bottom line:", "The data shows a big global success story with one clear warning: progress is not automatic. Where income stagnated and where disease or conflict struck, life expectancy stalled or fell."));

// 2 Objective
c.push(H1("2. Objective and approach"));
c.push(...bullets([
  "**Goal:** use R to build clear, honest charts that a non-technical reader can understand, and explain what each chart teaches.",
  "**Method:** each chart answers one question. I picked the chart type to match that question (distribution, comparison, relationship, change over time, outliers).",
  "**Chart types used:** histogram, bar chart, scatter plot, line charts, box plot, lollipop chart, small multiples, heatmap, base R plot and a lattice plot.",
  "**Reproducible:** one script (scripts/01_visualizations.R) creates every chart and every number quoted in this report.",
]));
c.push(label("Why this dataset?"));
c.push(...bullets([
  "It is public, well known and small enough to understand fully.",
  "It has all four things a visualization course needs: categories (continents), numbers (income, life expectancy), time (1952 to 2007) and size (population).",
  "It contains real surprises (wars, epidemics), which makes the \"anomalies\" part of the task meaningful.",
]));

// 3 Data overview
c.push(H1("3. Data overview"));
c.push(P("The data is the Gapminder dataset, a public collection of country-level health and wealth figures. The file used here comes from the open plotly/datasets repository on GitHub (gapminder_with_codes.csv). It has the same values as the gapminder package for R."));
c.push(H3("Size and quality checks"));
c.push(...bullets([
  "1,704 rows, 8 columns, 142 countries, 12 years (1952, 1957, ... 2007).",
  "No missing values in any column (checked with colSums(is.na(gap))).",
  "Each country appears exactly 12 times, so the data is balanced and easy to compare over time.",
]));
c.push(H3("Key variables"));
c.push(...table(["Variable", "Meaning", "Type"], [
  ["country", "Name of the country", "Text"],
  ["continent", "Africa, Americas, Asia, Europe or Oceania", "Category"],
  ["year", "Year of the reading (every 5 years)", "Number"],
  ["lifeExp", "Life expectancy at birth, in years", "Number"],
  ["pop", "Number of people living in the country", "Number"],
  ["gdpPercap", "Income per person (GDP per capita, US$, adjusted for inflation)", "Number"],
  ["iso_alpha, iso_num", "Standard country codes (not used in charts)", "Text / number"],
], [2200, 5560, 1600]));
c.push(H3("Things to keep in mind"));
c.push(...bullets([
  "**The data stops in 2007.** It cannot tell us about the last two decades.",
  "**Oceania has only 2 countries** (Australia and New Zealand). Its numbers should not be read as a whole-region picture.",
  "**Readings are every 5 years,** so short events can be missed or look smoothed.",
  "**Early-year figures are estimates.** For some countries, 1950s and 1960s numbers are less certain.",
  "**Continent labels follow the dataset.** For example, Réunion (a French territory) is listed under Africa.",
]));
c.push(label("R code: loading and checking the data"));
c.push(...codeBlock(section(2)));
c.push(label("Output of the code (first part of the console log)"));
c.push(...codeBlock(logHead));
c.push(callout("What this tells us:", "The data is clean and complete, so the charts below are not distorted by missing values. Life expectancy ranges from 23.6 to 82.6 years, and income per person from about $241 to $113,523. This huge income range is the reason a log scale is used in several charts."));

// 4 Setup
c.push(H1("4. Setup: packages, colours and theme"));
c.push(P("Before drawing anything, the script loads the packages and sets one colour per continent and one visual theme. Using the same colour for the same continent in every chart means the reader never has to re-learn the legend. The colours come from the Okabe-Ito palette, which stays readable for people with colour blindness."));
c.push(...codeBlock(section(1)));

// 5 Visualizations
c.push(new Paragraph({ children: [new PageBreak()] }));
c.push(H1("5. Visualizations, code and interpretation"));
c.push(P("Each part below follows the same pattern: **why this chart**, **the R code**, **the chart**, **what it shows** and **the insight**."));

function figure(num, title, why, file, caption, alt, shows, insight, caveat) {
  c.push(H2(`Figure ${num}: ${title}`));
  c.push(label("Why this chart?"));
  c.push(...bullets(why));
  c.push(label("R code"));
  c.push(...codeBlock(figCode(num)));
  c.push(...image(file, `Figure ${num}. ${caption}`, alt));
  c.push(label("What the chart shows"));
  c.push(...bullets(shows));
  c.push(callout("Insight:", insight));
  if (caveat) c.push(P("**Be careful:** " + caveat, { color: "7A3E00" }));
}

figure(1, "Histogram of life expectancy, 1952 vs 2007",
  [
    "A histogram counts how many countries fall into each range. It shows the whole picture, not only the average.",
    "Placing 1952 and 2007 on the same axes lets the reader see the whole group move to the right.",
  ],
  "fig01_histogram_lifeexp.png", "Number of countries by life expectancy, 1952 (grey) and 2007 (blue).",
  "Histogram comparing the distribution of country life expectancy in 1952 and 2007.",
  [
    "In 1952, most countries were between 30 and 50 years. **82 of 142 countries** were below 50.",
    "In 2007, only **19 countries** were below 50. The tallest bars are between 70 and 82.",
    "Countries with 70+ years: **5 in 1952, 83 in 2007.**",
    "Average rose from 49.1 to 67.0 years. The median (the middle country) rose from 45.1 to 71.9 years.",
  ],
  "This is one of the biggest improvements in human well-being in the data. The median rose more than the average because most countries improved a lot, while a small group at the bottom is still far behind.",
  "Bin width (2.5 years) is a design choice. Different widths change the look of the bars but not the conclusion.");

figure(2, "Bar chart: average life expectancy by continent, 2007",
  [
    "A bar chart is the simplest way to compare one number across a few groups.",
    "Bars are sorted from high to low and start at zero, so bar length matches the real size of the difference.",
    "Value labels remove the need to guess from the axis.",
  ],
  "fig02_bar_continent.png", "Average life expectancy of countries by continent, 2007.",
  "Horizontal bar chart of average life expectancy by continent in 2007.",
  [
    "Oceania 80.7, Europe 77.6, Americas 73.6, Asia 70.7, **Africa 54.8**.",
    "Africa is 22.8 years below Europe, and 15.9 years below Asia.",
  ],
  "Africa is far behind every other continent. The gap between Africa and the next lowest (Asia) is larger than the gap between Asia and Europe.",
  "Each country counts equally here, whether it has 1 million or 1 billion people. Oceania has only 2 countries, so its bar is not a continent-wide result.");

figure(3, "Scatter plot: income vs life expectancy, 2007",
  [
    "A scatter plot is the standard tool to see whether two numbers move together.",
    "Income is shown on a log scale. Without it, almost all countries would be squeezed into the left corner.",
    "Dot size shows population and colour shows continent, so four variables fit in one chart.",
    "A dashed smooth curve shows the general trend; labels point out interesting countries.",
  ],
  "fig03_scatter_gdp_life_2007.png", "Life expectancy vs income per person in 2007. Dot size is population.",
  "Scatter plot of life expectancy against GDP per capita on a log axis, coloured by continent.",
  [
    "Clear upward pattern: richer countries tend to have longer life expectancy.",
    "The curve flattens at high income. Going from $1,000 to $10,000 adds much more life than going from $30,000 to $40,000.",
    "Most African countries sit in the lower left. Most European countries sit in the upper right.",
    "**Above the trend:** Vietnam (74.2 years) and also Cuba (78.3), Costa Rica (78.8) and Albania (76.4) live longer than their income suggests.",
    "**Below the trend:** South Africa (49.3) and Swaziland (39.6) live much shorter than their income suggests.",
  ],
  "Income matters, but it is not the whole story. Countries above the line show that health can be good at modest income. Countries below it show that something other than money (for example a health crisis) can pull life expectancy down.",
  "This chart shows a link, not a cause. Richer countries can pay for health care, but healthier people also produce more. Both directions are possible.");

figure(4, "Line chart: life expectancy over time by continent",
  [
    "A line chart is the best choice for change over time because the eye follows the slope.",
    "Values are weighted by population, so a huge country counts more than a tiny one. This gives a better picture of how the average person lives.",
  ],
  "fig04_line_trend_continent.png", "Population-weighted life expectancy by continent, 1952 to 2007.",
  "Line chart with five lines showing life expectancy rising over time for each continent.",
  [
    "Every continent ends higher than it started.",
    "**Asia: 42.9 to 69.4 years (+26.5)**, the steepest climb. The Asia-Europe gap shrank from 22.0 to 8.4 years.",
    "**Africa: 38.8 to 54.6 years (+15.8).** The line is almost flat after 1987: 52.8 in 1987, 53.3 in 2002. In the 35 years before that, it rose about 14 years.",
    "The Africa-Europe gap only moved from 26.1 to 23.3 years.",
    "The small dip in Asia around 1962 is China (see Figure 6).",
  ],
  "Asia closed most of its gap. Africa's progress nearly stopped for 15 years, and it has not caught up.",
  "The chart cannot explain why Africa slowed. Figure 6 and Figure 10 give clues, but they are not proof.");

figure(5, "Box plot: differences between countries inside each continent, 2007",
  [
    "An average hides the spread. A box plot shows the middle half of countries (the box), the median (the line) and unusual values.",
    "Adding one dot per country lets the reader see real countries behind the box.",
  ],
  "fig05_boxplot_continent.png", "Life expectancy of each country in 2007, grouped by continent.",
  "Box plots with jittered dots showing the spread of country life expectancy within each continent.",
  [
    "**Africa:** from 39.6 to 76.4 years, a range of 36.8 years (the top value is Réunion, a French territory).",
    "**Asia:** from 43.8 (Afghanistan) to 82.6 (Japan), a range of 38.8 years, the widest of all.",
    "**Europe:** from 71.8 to 81.8, a range of only 10.0 years.",
    "**Oceania:** only two dots, so the box is almost a flat line.",
  ],
  "A continent average is a weak guide. Saying \"Asia is 70.7\" hides that Asia contains both one of the longest-lived and one of the shortest-lived populations in the world. Policy and analysis should look at individual countries.");

figure(6, "Anomalies: the biggest drops in life expectancy",
  [
    "The goal is to find what does not follow the general upward trend.",
    "All 142 countries are drawn as faint grey lines. This gives the \"normal\" background. Four countries with sharp drops are drawn in colour, so they stand out immediately.",
    "The R code first calculates the change between each reading and the one before (lag), then picks the largest falls.",
  ],
  "fig06_anomalies_drops.png", "Life expectancy of all countries (grey) with four unusual cases highlighted.",
  "Line chart with grey lines for all countries and coloured lines for Rwanda, Zimbabwe, Cambodia and China.",
  [
    "**Rwanda:** fell 20.4 years in one step to 23.6 in 1992, the lowest value in the whole dataset.",
    "**Zimbabwe:** fell 13.6 years from 1992 to 1997, then fell again to 40.0 by 2002.",
    "**Cambodia:** fell 9.1 years to 31.2 in 1977.",
    "**China:** fell 6.0 years to 44.5 in 1962, then recovered quickly.",
    "**Across the dataset:** 20 countries had lower life expectancy in 2007 than in 1992. **17 are in Africa,** with the largest falls in Swaziland (-18.9), Lesotho (-17.1) and Zimbabwe (-16.9).",
  ],
  "The upward trend is not guaranteed. Wars, famines and epidemics can wipe out decades of progress very fast. This is invisible in the continent averages of Figures 2 and 4.",
  "The data only shows that these drops happened. The commonly cited causes (civil war and genocide in Rwanda, HIV/AIDS in southern Africa, the Khmer Rouge period in Cambodia, famine in China in the early 1960s) come from outside knowledge, not from this dataset.");
c.push(label("The eight largest single-step drops (from the R output)"));
c.push(...table(["Country", "Continent", "Year", "Change (years)", "Life expectancy after drop"], [
  ["Rwanda", "Africa", "1992", "-20.4", "23.6"],
  ["Zimbabwe", "Africa", "1997", "-13.6", "46.8"],
  ["Lesotho", "Africa", "2002", "-11.0", "44.6"],
  ["Swaziland", "Africa", "2002", "-10.4", "43.9"],
  ["Botswana", "Africa", "1997", "-10.2", "52.6"],
  ["Cambodia", "Asia", "1977", "-9.1", "31.2"],
  ["Namibia", "Africa", "2002", "-7.4", "51.5"],
  ["South Africa", "Africa", "2002", "-6.9", "53.4"],
], [2200, 1700, 1100, 1960, 2400]));

figure(7, "Lollipop chart: the ten biggest gains, 1952 to 2007",
  [
    "A lollipop chart shows \"before and after\" for each country. The line length is the improvement.",
    "It is easier to read than two side-by-side bars per country.",
  ],
  "fig07_lollipop_gain.png", "Ten countries with the largest gain in life expectancy between 1952 and 2007.",
  "Lollipop chart with grey 1952 dots and coloured 2007 dots connected by a line for ten countries.",
  [
    "**Oman gained 38.1 years** (37.6 to 75.6). Next are Vietnam (+33.8), Indonesia (+33.2) and Saudi Arabia (+32.9).",
    "7 of the 10 are in Asia, 2 are in Africa (Libya, Gambia) and 1 is in the Americas (Nicaragua).",
    "South Korea gained 31.2 years and reached 78.6, the highest final level in this group.",
  ],
  "Large gains are possible, and they are not limited to rich countries. Africa also has success stories.",
  "These countries started very low, so they had the most room to grow. A big gain does not mean a high final level: Gambia gained 29.4 years and is still at 59.4.");

figure(8, "Small multiples: the income-health cloud in four snapshots",
  [
    "Instead of an animation, the same scatter plot is repeated for 1952, 1972, 1992 and 2007.",
    "All panels use the same axes, so the eye can compare them directly.",
    "Animation would not work in a Word document or a printout.",
  ],
  "fig08_small_multiples.png", "Life expectancy vs income in four years.",
  "Four scatter plot panels for 1952, 1972, 1992 and 2007 coloured by continent.",
  [
    "In 1952 the cloud is low and spread out. Most points are below 60 years and below $5,000.",
    "By 2007 the whole cloud has moved up and to the right, and the top of the chart is crowded.",
    "The African group (orange) moves up less than the others, and stays in the lower left in 2007.",
  ],
  "The \"low income, short life\" group of 1952 has mostly moved. The remaining group at the bottom is mainly African. The world did not just move on average, it also changed shape.");

figure(9, "Heatmap: the 15 most populous countries over time",
  [
    "A heatmap is useful when many rows and many time points must be compared in a small space.",
    "Colour shows the level, and printed numbers give the exact values.",
    "Looking only at the largest countries focuses on where most people live.",
  ],
  "fig09_heatmap_top15.png", "Life expectancy of the 15 most populous countries, 1952 to 2007.",
  "Heatmap with countries as rows and years as columns, coloured from light to dark by life expectancy.",
  [
    "**Japan:** 63 to 83 years, the highest in the group.",
    "**China:** 44 to 73. The big jump comes between 1962 (45) and 1967 (58).",
    "**India:** 37 to 65. **Ethiopia:** 34 to 53.",
    "**Nigeria:** stays at about 47 for four readings in a row (1992 to 2007). It is the only country in this group that looks stuck.",
    "**United States:** 68 to 78, behind Japan (83) and Germany (79) in 2007.",
  ],
  "Some very large countries improved massively (China, India, Indonesia, Bangladesh), which is why world-level life expectancy rose so much. Nigeria is the outlier of stagnation.");

figure(10, "Line chart: income growth by continent",
  [
    "This chart tests the income side of the story over time, using the same chart type as Figure 4 so the two can be compared.",
    "A log scale is used so that equal distances mean equal percentage growth.",
    "The median country is used so that one very rich country does not pull the line up.",
  ],
  "fig10_line_income.png", "Median income per person by continent, 1952 to 2007 (log scale).",
  "Line chart with five lines showing median GDP per capita rising by continent on a log axis.",
  [
    "Europe grew about **5.5 times** ($5,142 to $28,054). Asia grew about 3.7 times ($1,207 to $4,471). The Americas grew about 2.9 times.",
    "**Africa grew only about 47%** ($987 to $1,452) in 55 years, and is almost flat after the early 1970s.",
    "The income gap between Africa and Europe grew from about 5 times to about 19 times.",
  ],
  "Africa's stalled life expectancy in Figure 4 happened in the same period when its income was also stalled. The two charts fit together.",
  "\"Happened together\" is not proof that one caused the other. The HIV/AIDS epidemic and conflicts hit African countries in this period and affect both income and health.");

figure(11, "Base R: simple scatter plot with a regression line",
  [
    "The task allows ggplot2, lattice or base R. This chart is made with base R to show that the same message can be made with a different tool.",
    "A regression line turns the visual pattern into a number.",
  ],
  "fig11_base_r_scatter.png", "Base R scatter plot of life expectancy against log income with a fitted line (2007).",
  "Base R scatter plot with coloured dots and a dashed regression line.",
  [
    "The correlation between life expectancy and log income is **0.81**.",
    "**R-squared is 0.65:** income explains about 65% of the differences in life expectancy between countries in 2007.",
    "The slope is **16.6 years for every 10 times increase in income.** For example, moving from $1,000 to $10,000.",
    "Dots far below the line are mostly African.",
  ],
  "Income is a strong signal but leaves about 35% unexplained. That remaining part is where health policy, disease, conflict and education sit.",
  "This is one year and one simple model. It is a description, not a prediction or a proof of cause.");

figure(12, "Lattice: one small box plot panel per continent over time",
  [
    "The lattice package draws one panel per group automatically. It is another standard R plotting system, so it is included to show the range of R tools.",
    "It shows how the whole distribution changes each year, not only the average.",
  ],
  "fig12_lattice_boxplots.png", "Box plots of life expectancy by year, one panel per continent.",
  "Lattice panels, one per continent, showing box plots for each year from 1952 to 2007.",
  [
    "**Europe:** boxes are narrow and move up slowly. Low outliers in the early years join the main group by the 1990s.",
    "**Asia:** the median rises strongly, but a few low outliers stay far below the rest for decades.",
    "**Africa:** wide boxes in every year, with a median that flattens after 1987. The lowest dot is Rwanda in 1992.",
    "**Oceania:** a single tight line, because there are only two countries.",
  ],
  "Gaps between countries have shrunk in Europe, but not in Africa or Asia. This agrees with Figure 5.");

// 6 Synthesis
c.push(H1("6. Putting it all together"));
c.push(P("The 12 charts form one story. Each chart is a step in the argument:"));
c.push(...table(["Step", "Question", "Chart", "Answer"], [
  ["1", "Did the world change?", "Fig 1", "Yes, a very large upward shift in life expectancy."],
  ["2", "Who is ahead and behind?", "Fig 2, 5, 12", "Africa is behind, and the spread inside continents is large."],
  ["3", "Is money linked to long life?", "Fig 3, 8, 11", "Yes, strongly, but not completely (R-squared 0.65)."],
  ["4", "How did it change over time?", "Fig 4, 9, 10", "Asia caught up. Africa stalled when its income stalled."],
  ["5", "What went wrong, what went right?", "Fig 6, 7", "Shocks erased progress in some countries. Big gains happened in others."],
], [700, 2900, 1600, 4160]));

c.push(H2("What I would tell a decision maker"));
c.push(...bullets([
  "Do not use continent averages for decisions. Look at the country.",
  "Rising income helps, but countries with moderate income (Cuba, Costa Rica, Vietnam) show that good health outcomes are possible without being rich.",
  "Watch for shocks. A single event can undo decades of improvement within five years.",
  "Large countries with stalled progress (Nigeria in this data) deserve special attention because they affect many people.",
]));

c.push(H2("Limits of this analysis (honest list)"));
c.push(...bullets([
  "**Old data:** ends in 2007, so conclusions describe the past, not today.",
  "**Association only:** no chart here proves that income causes longer life. I did not run any statistical test of causes.",
  "**Averages and medians simplify:** countries are treated equally in some charts and population-weighted in others. This is stated in each title or subtitle, but readers could still mix them up.",
  "**Very small groups:** Oceania has 2 countries.",
  "**Explanations come from outside:** the reasons for the big drops are widely documented but are not in this dataset.",
  "**Five-year steps:** events shorter than five years can be missed.",
]));

c.push(H2("Possible next steps"));
c.push(...bullets([
  "Add newer data (after 2007) to see whether Africa's stall continued or ended.",
  "Add variables such as health spending, education or HIV rates, and test which ones explain the gaps in Figure 3.",
  "Build an interactive version with plotly or Shiny, so readers can hover and filter.",
]));

// 7 Evaluation mapping
c.push(H1("7. How this report meets the task criteria"));
c.push(...table(["Criterion", "Where it is covered"], [
  ["Creativity and relevance of visualizations", "12 chart types answer one clear question (Section 6). Highlighted anomalies (Fig 6), small multiples (Fig 8) and a heatmap (Fig 9) go beyond basic charts."],
  ["Clarity of explanations and narrative", "Every figure has: why this chart, what it shows, insight and caution. Language is kept simple. Section 1 gives a summary for non-technical readers."],
  ["Technical accuracy and quality of R code", "One script, commented, reproducible, with a shared theme and palette. Numbers in this report are printed by the script (figures/summary_stats.txt)."],
  ["Effectiveness in communicating insights", "Chart titles state the message. Colours are consistent and colour-blind friendly. Labels point out the key countries."],
], [3000, 6360]));

// Appendix
c.push(H1("Appendix A. GitHub repository and how to run"));
c.push(...codeBlock(`gapminder-r-visualization/
|-- README.md                    project description and how to run
|-- .gitignore
|-- data/
|   \`-- gapminder.csv            public dataset (1,704 rows)
|-- scripts/
|   \`-- 01_visualizations.R      creates all 12 figures and summary numbers
|-- figures/                     12 PNG charts + summary_stats.txt
|-- output/
|   |-- console_log.txt          console output of the last run
|   \`-- session_info.txt         R and package versions
\`-- report/
    |-- Week2_R_Data_Visualization_Report.docx
    \`-- build_report.js          script that builds this Word report`));
c.push(label("To reproduce the charts"));
c.push(...codeBlock(`# 1. Install the packages once (in R)
install.packages(c("ggplot2", "dplyr", "tidyr", "scales", "ggrepel",
                   "patchwork", "readr", "ragg", "viridis"))

# 2. From the project folder, run in a terminal
Rscript scripts/01_visualizations.R`));
c.push(label("Environment used"));
c.push(...codeBlock(fs.readFileSync(path.join(ROOT, "output", "session_info.txt"), "utf8")));

// ---------- build ----------
const doc = new Document({
  creator: "Student", title: "Week 2: Data Visualization and Insight Communication using R",
  styles: {
    default: { document: { run: { font: FONT, size: 22 } } },
    paragraphStyles: [
      { id: "Title", name: "Title", basedOn: "Normal", next: "Normal", run: { font: FONT, size: 52, bold: true, color: "111111" }, paragraph: { spacing: { before: 0, after: 160 } } },
      { id: "Heading1", name: "Heading 1", basedOn: "Normal", next: "Normal", quickFormat: true,
        run: { font: FONT, size: 34, bold: true, color: "0B4F8A" },
        paragraph: { spacing: { before: 360, after: 160 }, keepNext: true, outlineLevel: 0,
          border: { bottom: { style: BorderStyle.SINGLE, size: 8, color: "0072B2", space: 4 } } } },
      { id: "Heading2", name: "Heading 2", basedOn: "Normal", next: "Normal", quickFormat: true,
        run: { font: FONT, size: 28, bold: true, color: "111111" },
        paragraph: { spacing: { before: 400, after: 120 }, keepNext: true, outlineLevel: 1 } },
      { id: "Heading3", name: "Heading 3", basedOn: "Normal", next: "Normal", quickFormat: true,
        run: { font: FONT, size: 24, bold: true, color: "333333" },
        paragraph: { spacing: { before: 240, after: 100 }, keepNext: true, outlineLevel: 2 } },
    ],
  },
  numbering: { config: [{ reference: "bul", levels: [{ level: 0, format: LevelFormat.BULLET, text: "\u2022", alignment: AlignmentType.LEFT,
    style: { paragraph: { indent: { left: 540, hanging: 270 } } } }] }] },
  sections: [{
    properties: { page: { size: { width: 12240, height: 15840 }, margin: { top: 1440, right: 1440, bottom: 1300, left: 1440 } } },
    footers: { default: new Footer({ children: [new Paragraph({ alignment: AlignmentType.CENTER,
      children: [new TextRun({ children: ["Data Visualization using R  |  Page ", PageNumber.CURRENT], font: FONT, size: 16, color: "777777" })] })] }) },
    children: c,
  }],
});

const out = process.argv[2] || path.join(ROOT, "report", "Week2_R_Data_Visualization_Report.docx");
Packer.toBuffer(doc).then((buf) => { fs.writeFileSync(out, buf); console.log("wrote", out, buf.length); });
