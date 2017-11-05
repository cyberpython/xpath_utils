/* Copyright 2017 Georgios Migdos                                             */
/* 
/* Permission is hereby granted, free of charge, to any person obtaining a    */
/* copy of this software and associated documentation files (the "Software"), */
/* to deal in the Software without restriction, including without limitation  */
/* the rights to use, copy, modify, merge, publish, distribute, sublicense,   */
/* and/or sell copies of the Software, and to permit persons to whom the      */
/* Software is furnished to do so, subject to the following conditions:       */
/*                                                                            */
/* The above copyright notice and this permission notice shall be included in */
/* all copies or substantial portions of the Software.                        */
/*                                                                            */
/* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR */
/* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   */
/* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    */
/* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER */
/* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    */
/* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        */
/* DEALINGS IN THE SOFTWARE.                                                  */

#include <libxml/parser.h>
#include <libxml/tree.h>
#include <libxml/xpath.h>
#include <libxml/xmlerror.h>

#define XPATH_PROC_RET_CODE_SUCCESS 0
#define XPATH_PROC_RET_CODE_FAILURE 1
#define XPATH_PROC_RET_CODE_FAILED_TO_PARSE_FILE 2
#define XPATH_PROC_RET_CODE_XPATH_CONTEXT_CREATION_FAILED 3
#define XPATH_PROC_RET_CODE_INVALID_XPATH_EXPR 4

static void	dummyXmlGenericErrorFunc(void * ctx, const char * msg, ...)
{
  // do nothing - used to suppress libxml2 error printing to stderr
}

static void get_first_result_value(xmlNodeSetPtr nodes, char **result)
{
    xmlNodePtr cur = NULL;
    int size = 0;
    int i = 0;
    
    size = (nodes) ? nodes->nodeNr : 0;

    if (size > 0) {
      if (nodes->nodeTab[0])
      {
        cur = nodes->nodeTab[0];
        if(cur->type == XML_ELEMENT_NODE || cur->type == XML_ATTRIBUTE_NODE)
        {
          if(cur->children)
          {
            if(cur->children->content)
            {
              *result =  cur->children->content;
            }
          }
        } else 
        {
          *result = NULL;
        }
      }
    }
}


void xpath_proc_initialize()
{
  LIBXML_TEST_VERSION
  
  xmlInitParser();
}

void xpath_proc_finalize()
{
  xmlCleanupParser();
}

void xpath_proc_parse(char* filename, int suppress_error_msgs, xmlDoc **xml_doc, xmlXPathContext **xpath_context, int *ret_code)
{
  
  *xml_doc = NULL;
  *xpath_context = NULL; 
  *ret_code = XPATH_PROC_RET_CODE_FAILURE;

  if (suppress_error_msgs == 1)
  {
    xmlSetGenericErrorFunc(NULL, dummyXmlGenericErrorFunc);
  }

  *xml_doc = xmlReadFile(filename, NULL, 0);
  
  if(*xml_doc)
  {
    *xpath_context = xmlXPathNewContext(*xml_doc);
    if (*xpath_context)
    {
      *ret_code = XPATH_PROC_RET_CODE_SUCCESS;
    }
    else 
    {
      *ret_code = XPATH_PROC_RET_CODE_XPATH_CONTEXT_CREATION_FAILED;
    }
  }
  else
  {
    *ret_code = XPATH_PROC_RET_CODE_FAILED_TO_PARSE_FILE;
  }
}

void xpath_proc_cleanup(xmlDoc *xml_doc, xmlXPathContext *xpath_context)
{
  if(xpath_context)
  {
    xmlXPathFreeContext(xpath_context);
  }
  if(xml_doc)
  {
    xmlFreeDoc(xml_doc);
  }
}


void xpath_proc_get_value(const char *xpath_expr, xmlXPathContext *xpath_context, char **result, int *ret_code)
{

  xmlXPathObject *xpathObj = NULL;

  *result = NULL;
  *ret_code = XPATH_PROC_RET_CODE_FAILURE;
  
  xpathObj = xmlXPathEvalExpression(xpath_expr, xpath_context);
  
  if (xpathObj)
  {
    get_first_result_value(xpathObj->nodesetval, result);
    if(*result)
    {
      *ret_code = XPATH_PROC_RET_CODE_SUCCESS;
    }
    xmlXPathFreeObject(xpathObj);  
  }
  else 
  {
    *ret_code = XPATH_PROC_RET_CODE_INVALID_XPATH_EXPR;
  }

}
