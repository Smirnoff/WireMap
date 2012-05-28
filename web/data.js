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

function removeChildren(x)
{
  if (x.hasChildNodes())
    {
      while (x.childNodes.length > 0)
        {
          x.removeChild(x.firstChild);
        }
    }
}

function clear_data_results()
{
  var results = document.getElementById('results');
  removeChildren(results);
}

function set_data_results(msgs)
{
  var results = document.getElementById('results');
  msgs.forEach(
    function(msg)
      {
        results.appendChild(element_from_string('<p>' + msg + '</p>'));
      });
}

function list_map(func, lst)
{
  var res = [];
  lst.forEach(function(x){res.push(func(x));});
  return res;
}

var d; // debug
function click_check_data()
{
  var data = document.getElementById("data").value;
  check_data(
    data,
    function(res)
      {
        clear_data_results();
        d = res;// debug
        set_data_results(list_map(function(x){return x[1];}, res.messages));
      }
  );
}
