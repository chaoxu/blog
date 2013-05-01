MathJax.Hub.Config({
  showMathMenu: false,
  messageStyle: "none",
    tex2jax: {
      inlineMath: [ ['$','$'], ['\\(','\\)'] ],
      processEscapes: true,
    },
  "HTML-CSS": {
    availableFonts: ["STIX","TeX"],
    preferredFont: "STIX",
    webFont: "STIX",
    scale: 96
  },
  TeX: { 
    Macros: { 
        R: '{\\mathbb{R}}', 
        N: '{\\mathbb{N}}', 
        Z: '{\\mathbb{Z}}', 
        C: '{\\mathbb{C}}', 
        F: '{\\mathbb{F}}', 
        mex: '{\\mathop{\\operatorname{mex}}}', 
        lcm: '{\\mathop{\\operatorname{lcm}}}', 
     } 
    }, 
});