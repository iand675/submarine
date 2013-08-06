'use strict'

angular.module('frontendApp')
  .controller 'MainCtrl', ($scope) ->
    $scope.activeList = {
      tasks: [
        'HTML5 Boilerplate',
        'AngularJS',
        'Karma'
      ]
    }
