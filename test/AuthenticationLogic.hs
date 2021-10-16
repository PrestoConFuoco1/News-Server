module AuthenticationLogic where


import Execute
import qualified Exceptions as Ex
import Types
import Handlers
import App.Database
import Execute.Utils

import Test.Hspec

emulatedLogic :: Spec
emulatedLogic = do
    authorizedLogic
    adminPermissionsLogic



authorizedLogic :: Spec
authorizedLogic = do
  let def = defaultHandler
      nouser = def {getUserByToken = noUserByToken }
  describe "Authorization logic" $ do
    it "throws unauthorized exception if no token supplied" $ do
        (withAuth nouser Nothing >>= maybeUserToUser nouser)
            `shouldThrow` unauthorizedSelector
--    it "throws unauthorized exception if no token supplied" $ do
--        (withAuth nouser Nothing >>= getUser nouser)
--            `shouldThrow` unauthorizedSelector
    it "throws undefined on undefined token" $ do
        (withAuth nouser undefined >>= maybeUserToUser nouser)
            `shouldThrow` anyErrorCall



adminPermissionsLogic :: Spec
adminPermissionsLogic = do
  let def = defaultHandler
  describe "Permissions logic" $ do
    it "throws forbidden exception if no token supplied" $ do
        withAuthAdmin def Nothing
            `shouldThrow` forbiddenSelector
    it "throws forbidden exception if no user found with this token" $ do
        withAuthAdmin def { getUserByToken = noUserByToken } (Just "asdasdasda")
            `shouldThrow` forbiddenSelector
    it "throws forbidden exception if user is not admin" $ do
        withAuthAdmin def { getUserByToken = notAdminByToken } (Just "asdasdasda")
            `shouldThrow` forbiddenSelector
    it "returns unit-type value if user is admin" $ do
        withAuthAdmin def { getUserByToken = adminByToken } (Just "asdasdasda")
            `shouldReturn` ()

    it "throws undefined on undefined token" $ do
        withAuthAdmin def { getUserByToken = noUserByToken } undefined
            `shouldThrow` anyErrorCall

forbiddenSelector = serverErrorSelector Ex.Forbidden
unauthorizedSelector = serverErrorSelector Ex.Unauthorized

serverErrorSelector :: Ex.ServerException -> Selector Ex.ServerException
serverErrorSelector e = (== e)


