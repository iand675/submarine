'use strict';

describe('Controller: TasklistCtrl', function () {

  // load the controller's module
  beforeEach(module('frontendApp'));

  var TasklistCtrl,
    scope;

  // Initialize the controller and a mock scope
  beforeEach(inject(function ($controller, $rootScope) {
    scope = $rootScope.$new();
    TasklistCtrl = $controller('TasklistCtrl', {
      $scope: scope
    });
  }));

  it('should attach a list of awesomeThings to the scope', function () {
    expect(scope.awesomeThings.length).toBe(3);
  });
});
