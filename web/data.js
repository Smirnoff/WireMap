function check_data(data, res, err)
{
  var been_called = false;
  post_to_url('check_data.scm',
              function(x)
                {
                  if (x.length != 0 && !been_called)
                    {
                      been_called = true;
                      res(JSON.parse(x));
                    }
                },
              JSON.stringify({data: data}),
              err);
}

function find_item(node, func)
{
  var res = [];
}

function clear_data_results()
{
  for (var node in document.getElementById('results').childNodes)
    {
      if (node.nodeName != "#text" && typeof(node) != "string")
        { element_set_by_id(node.id, ""); } }
}

function click_check_data()
{
  var data = document.getElementById("data").value;
  check_data(
    data,
    function(res)
      {
        clear_data_results();
        res.messages.forEach(
          function(item){element_set_by_id(item[0], item[1]);});
      }
  );
}
