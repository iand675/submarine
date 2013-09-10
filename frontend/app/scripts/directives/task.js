'use strict';

angular.module('frontendApp')
  .directive('task', function () {
    return {
      template: '<div></div>',
      restrict: 'E',
      link: function postLink(scope, element, attrs) {
        element.text('this is the task directive');
      }
    };
  });
