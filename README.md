# injecthtml

Inject html layout templates using shell file arguments and XPath expressions.

Usage


    injecthtml
    
    Usage: injecthtml (TEMPLATE-FILE | -e TEMPLATE-STRING) ([-s STRING[OP]XPATH] |
                      [-f FILE[OP]XPATH])
      OP expressions are >>[XPATH] to insert children and ^^[XPATH] to replace node
    
    Available options:
      -h,--help                Show this help text
      TEMPLATE-FILE            Template file path
      -e TEMPLATE-STRING       Template as inline string
      -f FILE[OP]XPATH         Use -[OP]XPATH to use STDIN


    # template.html
    <div>
      <h2>HELLO</h2>
      <p class='content'>
        <span></span>
      </p>
    </div>

    # command; the >> operator replaces child:
    echo -n 'WORLD' | injecthtml template.html  -f '->>//p[1]/span' 

    # output:
    <div>
      <h2>HELLO</h2>
      <p class="content">
        <span>WORLD</span>
      </p>
    </div>

    # command; the ^^ operator replaces node:
    echo -n 'WORLD' | injecthtml template.html  -f '-^^//p[1]/span' 

    <div>
      <h2>HELLO</h2>
      <p class="content">
        WORLD
      </p>
    </div>
