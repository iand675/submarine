'use strict'

angular.module('frontendApp')
  .controller 'MainCtrl', ($scope) ->
    currentTaskId = 0
    $scope.taskFilter = ''
    $scope.activeList = {
      name: 'Inbox',
      tasks: []
    }
    $scope.toggleTaskCompletion = (taskId) ->
      selectedTask = _($scope.activeList.tasks).find (t) ->
        t.id == taskId

      if selectedTask
        selectedTask.complete = !selectedTask.complete

    $scope.newTaskName = ''
    $scope.addTask = () ->
      $scope.activeList.tasks.push {
        id: currentTaskId,
        name: $scope.newTaskName,
        tags: ['tag', 'tag2'],
        complete: no
      }
      $scope.newTaskName = ''
      currentTaskId++

