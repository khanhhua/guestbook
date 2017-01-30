(function ($) {
  $.fn.api.settings.api = {
    'leave message': '/rest/messages'
  };

  $('#modalContact .ui.send.button').api({
    action: 'leave message',
    method : 'POST',
    beforeSend: function (settings) {
      settings.contentType = 'application/json';
      settings.data = JSON.stringify({
        name:    $('#modalContact [name=username]').val(),
        contact: $('#modalContact [name=contact]').val(),
        message: $('#modalContact [name=message]').val()
      });

      return settings;
    },
    onSuccess: function () {
      $('#modalContact').modal('hide');
    }
  });
  $('.button.leave-message').on('click', function () {
    $('#modalContact').modal('show');
  });

  $(renderMessageList);

  function renderMessageList () {
    var templateFn;
    var $messageList;
    var timer;

    $.get('/partials/message.item.html').then(function (template) {
      console.log(template);

      templateFn = _.template(template);
      $messageList = $('.message-list');

      syncMessages().then(render).then(function () {
        connect(render);
      });
    });

    function render(data) {
      var $html = _.map(data.messages, function (item) {
        item.username = item.guest_id;
        return templateFn(item);
      });

      $html.forEach(function (item) {
        $(item).prependTo($messageList).fadeIn();
      });
    }
  }

  function syncMessages () {
    return $.get('/rest/messages').then(function (data) {
      return data;
    });
  }

  function connect (render) {
    var eventSource = new EventSource('/sse');
    eventSource.addEventListener('heartbeat', function () {
      console.info('[hearbeat] Life is good');
    });

    eventSource.addEventListener('change', function (e) {
      var data = {};
      try {
        data.messages = JSON.parse(e.data);
        render(data);
      }
      catch (e) {}
    });
  }
})(jQuery);
