module Routing where

import Test.Hspec

import Action.RequestToAction
import Action.Common
import Types
import Data.Text as T
import Action.Utils
import qualified Utils as S

invalidEndpoint = WhoWhat Nothing $ ActionErrorPerms False EInvalidEndpoint

testRouting :: Spec
testRouting = do
    describe "test routing" $ do
        let singletonPaths = ["wtf", "posts", "drafts", "categories", "authors", "tags", "users", "comments"]
            invalidSingleton str =
                it ("returns invalid endpoint error on invalid singleton path: " <> T.unpack str)  $
                (requestToAction [str] []) `shouldBe` Left invalidEndpoint

        it "returns invalid endpoint error on empty path" $
            (requestToAction [] []) `shouldBe` Left invalidEndpoint
        mapM_ invalidSingleton singletonPaths

        it "correctly handles authentication" $
            (requestToAction ["auth"] [("login", Just "login1"), ("pass_hash", Just "password1")])
            `shouldBe` Right (WhoWhat Nothing $ AAuth $ Authenticate {
                auLogin = "login1"
                , auPassHash = "password1"
                })

        it "returns invalid endpoint error on extra path" $
            (requestToAction ["auth", "extra"] [])
            `shouldBe` Left invalidEndpoint

        testPostsRouting
        testDraftRouting
        testCategoriesRouting
        testTagsRouting
        testUsersRouting
        testAuthorsRouting
        testCommentsRouting


testPostsRouting :: Spec
testPostsRouting = do
    describe "posts routing" $ do
        it "correctly handles publish" $
            (requestToAction ["publish"] [("draft_id", Just "3")]) `shouldBe`
            Right (WhoWhat Nothing $ APublish $ Publish 3)

        it "correctly handles getting posts" $ 
            (requestToAction ["posts", "get"]
                [("tags__in", Just "[1,2,3]"),
                 ("created_at__lt", Just "2018-09-30"),
                 ("sort", Just "da"),
                 ("search", Just "haha")])
            `shouldBe` Right (WhoWhat Nothing $ APosts $ AP $ Read $ Paginated defaultPage defaultSize $
                GetPosts {
                    gpCreationDate = fmap CreatedEarlier $ S.readDay "2018-09-30"
                    , gpTags = Just $ TagsIn [1,2,3]
                    , gpSearch = Just $ SearchOptions "haha"
                    , gpSort = SortOptions SEDate SOAscending
                })

        it "return invalid endpoint for extra path unit" $ 
            (requestToAction ["posts", "get", undefined] []) `shouldBe` Left invalidEndpoint

        it "correctly handles getting post comments" $
            requestToAction ["posts", "123", "comments"] [] `shouldBe`
            Right (WhoWhat Nothing $ APosts $ GC $ Paginated defaultPage defaultSize $ GetComments 123)
 

testDraftRouting :: Spec
testDraftRouting = do
    describe "draft routing" $ do
        it "correctly handles getting drafts" $
            requestToAction ["drafts", "get"] [] `shouldBe`
            Right (WhoWhat Nothing $ ADrafts $ Read $ Paginated defaultPage defaultSize $ GetDrafts)

        it "correctly handles creating draft" $
            requestToAction ["drafts", "create"]
            [("title", Just "title1"), ("tags", Just "[4,5,6]"),
             ("category_id", Just "2"), ("content", Just "halo!")] `shouldBe`
             Right (WhoWhat Nothing $ ADrafts $ Create $ CreateDraft {
                cdTitle = "title1", cdTags = [4,5,6], cdCategoryId = 2,
                cdContent = "halo!", cdMainPhoto = Nothing, cdExtraPhotos = Nothing
                })

        it "correctly handles editing draft" $
            requestToAction ["drafts", "edit"]
            [("draft_id", Just "2"), ("title", Just "title2"), ("tags", Just "[5,6,7]"),
             ("category_id", Just "3"), ("content", Just "halo!")] `shouldBe`
             Right (WhoWhat Nothing $ ADrafts $ Update $ EditDraft {
                edDraftId = 2,
                edTitle = Just "title2", edTags = Just [5,6,7], edCategoryId = Just 3,
                edContent = Just "halo!", edMainPhoto = Nothing, edExtraPhotos = Nothing
                })

        it "correctly handles deleting draft" $
            requestToAction ["drafts", "delete"]
            [("draft_id", Just "2")] `shouldBe` Right (WhoWhat Nothing $ ADrafts $ Delete $ DeleteDraft {
                ddDraftId = 2
                })

testCategoriesRouting :: Spec
testCategoriesRouting = do
    describe "categories routing" $ do
        it "correctly handles getting categories" $
            requestToAction ["categories", "get"] [] `shouldBe`
            Right (WhoWhat Nothing $ ACategory $ Read $ Paginated defaultPage defaultSize $ GetCategories Nothing)

        it "correctly handles creating categories" $
            requestToAction ["categories", "create"] [("name", Just "catName1"), ("parent_id", Just "2")]
            `shouldBe` Right (WhoWhat Nothing $ ACategory $ Create $ CreateCategory {
                ccCategoryName = "catName1",
                ccParentCategory = 2
                })

        it "correctly handles editing categories" $
            requestToAction ["categories", "edit"] [("category_id", Just "4"), ("name", Just "catName1"), ("parent_id", Just "2")]
            `shouldBe` Right (WhoWhat Nothing $ ACategory $ Update $ EditCategory {
                ecCategoryId = 4,
                ecCategoryName = Just "catName1",
                ecParentId = Just 2
                })

        it "correctly handles deleting categories" $
            requestToAction ["categories", "delete"] [("category_id", Just "5")] `shouldBe`
            Right (WhoWhat Nothing $ ACategory $ Delete $ DeleteCategory {
                dcCategoryId = 5
                })


testTagsRouting :: Spec
testTagsRouting = do
    describe "tags routing" $ do
        it "correctly handles getting tags" $
            requestToAction ["tags", "get"] [] `shouldBe`
            Right (WhoWhat Nothing $ ATags $ Read $ Paginated defaultPage defaultSize $ GetTags)

        it "correctly handles creating tags" $
            requestToAction ["tags", "create"] [("name", Just "tagName1")]
            `shouldBe` Right (WhoWhat Nothing $ ATags $ Create $ CreateTag {
                ctTagName = "tagName1"
                })

        it "correctly handles editing tag" $
            requestToAction ["tags", "edit"] [("tag_id", Just "4"), ("name", Just "tagName1")]
            `shouldBe` Right (WhoWhat Nothing $ ATags $ Update $ EditTag {
                etTagId = 4,
                etTagName = "tagName1"
                })

        it "correctly handles deleting tag" $
            requestToAction ["tags", "delete"] [("tag_id", Just "5")] `shouldBe`
            Right (WhoWhat Nothing $ ATags $ Delete $ DeleteTag {
                dtTagId = 5
                })




testUsersRouting :: Spec
testUsersRouting = do
    describe "users routing" $ do
        it "correctly handles getting profile" $
            requestToAction ["users", "profile"] [] `shouldBe`
            Right (WhoWhat Nothing $ AUsers $ Read GetProfile)

        it "correctly handles creating user" $
            requestToAction ["users", "create"]
            [("login", Just "login1"), ("pass_hash", Just "password"),
             ("firstname", Just "fn"), ("lastname", Just "ln")] `shouldBe`
            Right (WhoWhat Nothing $ AUsers $ Create $ CreateUser {
                cuLogin = "login1"
                , cuPassHash = "password"
                , cuFirstName = "fn"
                , cuLastName = "ln"
            })

        it "correctly handles deleting user" $
            requestToAction ["users", "delete"] [("user_id", Just "2")] `shouldBe`
            Right (WhoWhat Nothing $ AUsers $ Delete $ DeleteUser {
                duUserId = 2
                })


testAuthorsRouting :: Spec
testAuthorsRouting = do
    describe "authors routing" $ do
        it "correctly handles getting authors" $
            requestToAction ["authors", "get"] [] `shouldBe`
            Right (WhoWhat Nothing $ AAuthors $ Read $ Paginated defaultPage defaultSize $ GetAuthors Nothing)

        it "correctly handles creating authors" $
            requestToAction ["authors", "create"] [("user_id", Just "2"), ("description", Just "description1")] `shouldBe`
            Right (WhoWhat Nothing $ AAuthors $ Create $ CreateAuthor {
                caUserId = 2
                , caDescription = "description1"
                })


        it "correctly handles editing authors" $
            requestToAction ["authors", "edit"]
            [("author_id", Just "3"), ("user_id", Just "4"), ("description", Just "description1")]
            `shouldBe`
            Right (WhoWhat Nothing $ AAuthors $ Update $ EditAuthor {
                eaAuthorId = 3
                , eaUserId = Just 4
                , eaDescription = Just "description1"
                })

        it "correctly handles delete author" $
            requestToAction ["authors", "delete"]
            [("author_id", Just "6")] `shouldBe` Right (WhoWhat Nothing $AAuthors $ Delete $ DeleteAuthor {
                daAuthorId = 6
                })

testCommentsRouting :: Spec
testCommentsRouting = do
    describe "comments routing" $ do
        it "correctly handles getting comments" $
            requestToAction ["comments", "get"] [("post_id", Just "123")] `shouldBe`
            Right (WhoWhat Nothing $AComments $ Read $ Paginated defaultPage defaultSize $ GetComments 123)

        it "correctly handles creating comment" $
            requestToAction ["comments", "create"] [("post_id", Just "123"), ("content", Just "content1")] `shouldBe`
            Right (WhoWhat Nothing $ AComments $ Create $ CreateComment {
                ccomPostId = 123
                , ccomContent = "content1"
                })

        it "correctly handles deleting comment" $
            requestToAction ["comments", "delete"] [("comment_id", Just "3")] `shouldBe`
            Right (WhoWhat Nothing $ AComments $ Delete $ DeleteComment {
                dcCommentId = 3
                })


