'use strict';

angular.module('frontendApp')
  .controller('SigninCtrl', function ($scope, $modal) {
  	$scope.open = function () {
  		var modalInstance = $modal.open({
  			templateUrl: 'views/sign_in_or_register.html'
  		});
  	}
  });
