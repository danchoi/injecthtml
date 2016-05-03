# injecthtml

Inject html layout templates using shell file arguments and XPath expressions.

Usage


    injecthtml
    
    Usage: injecthtml (TEMPLATE-FILE | -e TEMPLATE-STRING) [-k SEPARATOR]
                      ([-s STRING@XPATH] | [-f FILE@XPATH])
      HTML template inject
    
    Available options:
      -h,--help                Show this help text
      TEMPLATE-FILE            Template file path
      -e TEMPLATE-STRING       Template as inline string
      -k SEPARATOR             Separator characters between FILE/STRING and XPATH.
                               Default ::


    # template.html
    <div>
      <h2>HELLO</h2>
      <p class='content'>
        <span></span>
      </p>
    </div>

    # command:
    echo -n 'WORLD' | injecthtml template.html  -f '-:://p[1]/span' 

    # output:
    <div>
      <h2>HELLO</h2>
      <p class="content">
        <span>WORLD</span>
      </p>
    </div>

