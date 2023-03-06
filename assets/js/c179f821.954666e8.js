"use strict";(self.webpackChunkmmm_for_all=self.webpackChunkmmm_for_all||[]).push([[699],{3905:function(e,t,r){r.d(t,{Zo:function(){return c},kt:function(){return h}});var n=r(7294);function a(e,t,r){return t in e?Object.defineProperty(e,t,{value:r,enumerable:!0,configurable:!0,writable:!0}):e[t]=r,e}function o(e,t){var r=Object.keys(e);if(Object.getOwnPropertySymbols){var n=Object.getOwnPropertySymbols(e);t&&(n=n.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),r.push.apply(r,n)}return r}function i(e){for(var t=1;t<arguments.length;t++){var r=null!=arguments[t]?arguments[t]:{};t%2?o(Object(r),!0).forEach((function(t){a(e,t,r[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(r)):o(Object(r)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(r,t))}))}return e}function l(e,t){if(null==e)return{};var r,n,a=function(e,t){if(null==e)return{};var r,n,a={},o=Object.keys(e);for(n=0;n<o.length;n++)r=o[n],t.indexOf(r)>=0||(a[r]=e[r]);return a}(e,t);if(Object.getOwnPropertySymbols){var o=Object.getOwnPropertySymbols(e);for(n=0;n<o.length;n++)r=o[n],t.indexOf(r)>=0||Object.prototype.propertyIsEnumerable.call(e,r)&&(a[r]=e[r])}return a}var s=n.createContext({}),p=function(e){var t=n.useContext(s),r=t;return e&&(r="function"==typeof e?e(t):i(i({},t),e)),r},c=function(e){var t=p(e.components);return n.createElement(s.Provider,{value:t},e.children)},u="mdxType",d={inlineCode:"code",wrapper:function(e){var t=e.children;return n.createElement(n.Fragment,{},t)}},m=n.forwardRef((function(e,t){var r=e.components,a=e.mdxType,o=e.originalType,s=e.parentName,c=l(e,["components","mdxType","originalType","parentName"]),u=p(r),m=a,h=u["".concat(s,".").concat(m)]||u[m]||d[m]||o;return r?n.createElement(h,i(i({ref:t},c),{},{components:r})):n.createElement(h,i({ref:t},c))}));function h(e,t){var r=arguments,a=t&&t.mdxType;if("string"==typeof e||a){var o=r.length,i=new Array(o);i[0]=m;var l={};for(var s in t)hasOwnProperty.call(t,s)&&(l[s]=t[s]);l.originalType=e,l[u]="string"==typeof e?e:a,i[1]=l;for(var p=2;p<o;p++)i[p]=r[p];return n.createElement.apply(null,i)}return n.createElement.apply(null,r)}m.displayName="MDXCreateElement"},9478:function(e,t,r){r.r(t),r.d(t,{assets:function(){return c},contentTitle:function(){return s},default:function(){return h},frontMatter:function(){return l},metadata:function(){return p},toc:function(){return u}});var n=r(7462),a=r(3366),o=(r(7294),r(3905)),i=(r(4996),["components"]),l={id:"quick-start",title:"Quick Start"},s=void 0,p={unversionedId:"quick-start",id:"quick-start",title:"Quick Start",description:"---",source:"@site/docs/quick-start.mdx",sourceDirName:".",slug:"/quick-start",permalink:"/Robyn/docs/quick-start",draft:!1,editUrl:"https://github.com/facebookexperimental/Robyn/edit/main/website/docs/quick-start.mdx",tags:[],version:"current",frontMatter:{id:"quick-start",title:"Quick Start"},sidebar:"someSidebar",next:{title:"Additional Materials",permalink:"/Robyn/docs/additional-materials"}},c={},u=[{value:"1. Downloading the latest R version",id:"1-downloading-the-latest-r-version",level:2},{value:"2. Installing the package",id:"2-installing-the-package",level:2},{value:"3. Getting started with the demo.R script",id:"3-getting-started-with-the-demor-script",level:2}],d={toc:u},m="wrapper";function h(e){var t=e.components,r=(0,a.Z)(e,i);return(0,o.kt)(m,(0,n.Z)({},d,r,{components:t,mdxType:"MDXLayout"}),(0,o.kt)("hr",null),(0,o.kt)("h2",{id:"1-downloading-the-latest-r-version"},"1. Downloading the latest R version"),(0,o.kt)("p",null,"It is necessary to have the ",(0,o.kt)("a",{parentName:"p",href:"https://www.r-project.org/"},(0,o.kt)("strong",{parentName:"a"},"R 4.0.0 version (or higher)"))," installed in order to be able to run this code. R is a free software environment for statistical computing and graphics. It compiles and runs on a wide variety of UNIX platforms, Windows and MacOS. To\n",(0,o.kt)("a",{parentName:"p",href:"https://cran.r-project.org/mirrors.html"},"download R."),", please choose your\npreferred ",(0,o.kt)("a",{parentName:"p",href:"https://cran.r-project.org/mirrors.html"},"CRAN mirror"),". ",(0,o.kt)("strong",{parentName:"p"},"Please make sure you have restarted your R session once you have installed the latest R version.")),(0,o.kt)("p",null,(0,o.kt)("strong",{parentName:"p"},"Optional (and recommended)"),": after you've installed R, you may install ",(0,o.kt)("a",{parentName:"p",href:"https://www.rstudio.com/products/rstudio/download/"},"RStudio IDE")," for a better and smoother experience. RStudio IDE is a set of integrated tools designed to help you be more productive with R and Python. It includes a console, syntax-highlighting editor that supports direct code execution, and a variety of robust tools for plotting, viewing history, debugging and managing your workspace."),(0,o.kt)("hr",null),(0,o.kt)("h2",{id:"2-installing-the-package"},"2. Installing the package"),(0,o.kt)("p",null,"To install or update Robyn to the latest stable version of the package, run:"),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre"},'install.packages("Robyn")\n')),(0,o.kt)("p",null,(0,o.kt)("strong",{parentName:"p"},"NOTE"),": If you're interested in installing the latest or a specific ",(0,o.kt)("strong",{parentName:"p"},"dev version")," instead, run ",(0,o.kt)("inlineCode",{parentName:"p"},'remotes::install_github("facebookexperimental/Robyn/R")'),". If you haven't installed the ",(0,o.kt)("inlineCode",{parentName:"p"},"remotes")," package previously, run ",(0,o.kt)("inlineCode",{parentName:"p"},"install.packages('remotes')")," first."),(0,o.kt)("p",null,"Robyn also requires the Python library ",(0,o.kt)("a",{parentName:"p",href:"https://facebookresearch.github.io/nevergrad/"},"Nevergrad"),".\n",(0,o.kt)("strong",{parentName:"p"},"You must install it once for the code to work properly"),". Please find ",(0,o.kt)("a",{parentName:"p",href:"https://rstudio.github.io/reticulate/articles/python_packages.html"},"here")," more info about installing Python packages via reticulate."),(0,o.kt)("ol",null,(0,o.kt)("li",{parentName:"ol"},"First step is to install the ",(0,o.kt)("a",{parentName:"li",href:"https://rstudio.github.io/reticulate/"},"reticulate package")," (if you haven't before) and load it:")),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre"},'install.packages("reticulate")\nlibrary(reticulate)\n')),(0,o.kt)("ol",{start:2},(0,o.kt)("li",{parentName:"ol"},"Install ",(0,o.kt)("inlineCode",{parentName:"li"},"nevergrad")," Python library. You have 2 options:")),(0,o.kt)("ul",null,(0,o.kt)("li",{parentName:"ul"},(0,o.kt)("strong",{parentName:"li"},"Option 1: using PIP"))),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre"},'virtualenv_create("r-reticulate")\npy_install("nevergrad", pip = TRUE)\nuse_virtualenv("r-reticulate", required = TRUE)\n')),(0,o.kt)("ul",null,(0,o.kt)("li",{parentName:"ul"},(0,o.kt)("strong",{parentName:"li"},"Option 2: using conda"))),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre"},'conda_create("r-reticulate")\nconda_install("r-reticulate", "nevergrad", pip = TRUE)\nuse_condaenv("r-reticulate")\n')),(0,o.kt)("p",null,"In case nevergrad still cannot be imported after installation, please locate your python file and run this line using your path:"),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre"},'use_python("~/Library/r-miniconda/envs/r-reticulate/bin/python")\n')),(0,o.kt)("hr",null),(0,o.kt)("h2",{id:"3-getting-started-with-the-demor-script"},"3. Getting started with the ",(0,o.kt)("a",{parentName:"h2",href:"https://github.com/facebookexperimental/Robyn/blob/main/demo/demo.R"},"demo.R")," script"),(0,o.kt)("p",null,"Open the ",(0,o.kt)("a",{parentName:"p",href:"https://github.com/facebookexperimental/Robyn/blob/main/demo/demo.R"},"demo.R")," script in the ",(0,o.kt)("inlineCode",{parentName:"p"},"Robyn/demo/")," folder as a quick start guide that aims to cover most common use-cases.\nThe demo will allow you to test the package using a simulated dataset."),(0,o.kt)("hr",null))}h.isMDXComponent=!0}}]);