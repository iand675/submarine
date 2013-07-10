module GitHub.Users.Emails where

emails = currentUser <> "/emails"

--| GET /user/emails
getCurrentUserEmails ::
	GitHub UserEmailsData
getCurrentUserEmails = ghGet [] emails

--| POST /user/emails
addEmailAddresses ::
	[Text] ->
	GitHub UserEmailsData
addEmailAddresses = ghPost emails

--| DELETE /user/emails
deleteEmailAddresses ::
	[Text] ->
	GitHub ()
deleteEmailAddresses = ghDelete' emails