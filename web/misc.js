function fetch_element_by_id(id)
{
  return document.getElementById(id);
}

function element_set_by_id(id, value)
{
  fetch_element_by_id(id).innerHTML = value;
}

function element_from_string(str)
{
  var div = document.createElement('div');
  div.innerHTML = str;
  if (div.childNodes.length != 1)
    throw new Error("Didn't get one childnode");

  return div.firstChild;
}

function request_url(type, url, callback, mime_type, err, data)
{
  if (err === undefined) err = function(val) { alert("Error: " + val); };

  if (data === undefined) data = null;

  $.ajax(
    {
      url: url,
      type: type,
      data: data,
      mimeType: mime_type,
      success: callback,
      error: err
    }
  );
}

function post_to_url(url, callback, data, err, mime_type)
{
  if (mime_type === undefined) mime_type = "text/plain";
  request_url('POST', url, callback, mime_type, err, data);
}

function array_for_each(array, func)
{
  var idx;
  for (idx = 0; idx < array.length; idx++)
    {
      func(array[idx]);
    }
}
