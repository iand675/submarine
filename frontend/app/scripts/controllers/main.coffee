'use strict'

angular.module('frontendApp')
  .controller 'MainCtrl', ($scope) ->
    currentTaskId = 0
    $scope.taskFilter = ''
    $scope.activeList = {
      name: 'Inbox',
      tasks: {}
    }
    $scope.toggleTaskCompletion = (taskId) ->
      $scope.activeList.tasks[taskId].complete = !$scope.activeList.tasks[taskId].complete

    $scope.newTaskName = ''
    $scope.addTask = () ->
      $scope.activeList.tasks[currentTaskId] = {
        name: $scope.newTaskName,
        complete: no
      }
      $scope.newTaskName = ''
      currentTaskId++

