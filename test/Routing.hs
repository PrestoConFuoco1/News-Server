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
                (requestToAction1 [str] []) `shouldBe` Left invalidEndpoint

        it "returns invalid endpoint error on empty path" $
            (requestToAction1 [] []) `shouldBe` Left invalidEndpoint
        mapM_ invalidSingleton singletonPaths

        it "correctly handles authentication" $
            (requestToAction1 ["auth"] [("login", Just "login1"), ("pass_hash", Just "password1")])
            `shouldBe` Right (WhoWhat Nothing $ AAuth $ Authenticate {
                _au_login = "login1"
                , _au_passHash = "password1"
                })

        it "returns invalid endpoint error on extra path" $
            (requestToAction1 ["auth", "extra"] [])
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
            (requestToAction1 ["publish"] [("draft_id", Just "3")]) `shouldBe`
            Right (WhoWhat Nothing $ APublish $ Publish 3)

        it "correctly handles getting posts" $ 
            (requestToAction1 ["posts", "get"]
                [("tags__in", Just "[1,2,3]"),
                 ("created_at__lt", Just "2018-09-30"),
                 ("sort", Just "da"),
                 ("search", Just "haha")])
            `shouldBe` Right (WhoWhat Nothing $ APosts $ AP $ Read $ Paginated defaultPage defaultSize $
                GetPosts {
                    _gp_creationDate = fmap CreatedEarlier $ S.readDay "2018-09-30"
                    , _gp_tags = Just $ TagsIn [1,2,3]
                    , _gp_search = Just $ SearchOptions "haha"
                    , _gp_sort = SortOptions SEDate SOAscending
                })

        it "return invalid endpoint for extra path unit" $ 
            (requestToAction1 ["posts", "get", undefined] []) `shouldBe` Left invalidEndpoint

        it "correctly handles getting post comments" $
            requestToAction1 ["posts", "123", "comments"] [] `shouldBe`
            Right (WhoWhat Nothing $ APosts $ GC $ Paginated defaultPage defaultSize $ GetComments 123)
 

testDraftRouting :: Spec
testDraftRouting = do
    describe "draft routing" $ do
        it "correctly handles getting drafts" $
            requestToAction1 ["drafts", "get"] [] `shouldBe`
            Right (WhoWhat Nothing $ ADrafts $ Read $ Paginated defaultPage defaultSize $ GetDrafts)

        it "correctly handles creating draft" $
            requestToAction1 ["drafts", "create"]
            [("title", Just "title1"), ("tags", Just "[4,5,6]"),
             ("category_id", Just "2"), ("content", Just "halo!")] `shouldBe`
             Right (WhoWhat Nothing $ ADrafts $ Create $ CreateDraft {
                _cd_title = "title1", _cd_tags = [4,5,6], _cd_categoryId = 2,
                _cd_content = "halo!", _cd_mainPhoto = Nothing, _cd_extraPhotos = Nothing
                })

        it "correctly handles editing draft" $
            requestToAction1 ["drafts", "edit"]
            [("draft_id", Just "2"), ("title", Just "title2"), ("tags", Just "[5,6,7]"),
             ("category_id", Just "3"), ("content", Just "halo!")] `shouldBe`
             Right (WhoWhat Nothing $ ADrafts $ Update $ EditDraft {
                _ed_draftId = 2,
                _ed_title = Just "title2", _ed_tags = Just [5,6,7], _ed_categoryId = Just 3,
                _ed_content = Just "halo!", _ed_mainPhoto = Nothing, _ed_extraPhotos = Nothing
                })

        it "correctly handles deleting draft" $
            requestToAction1 ["drafts", "delete"]
            [("draft_id", Just "2")] `shouldBe` Right (WhoWhat Nothing $ ADrafts $ Delete $ DeleteDraft {
                _dd_draft_id = 2
                })

testCategoriesRouting :: Spec
testCategoriesRouting = do
    describe "categories routing" $ do
        it "correctly handles getting categories" $
            requestToAction1 ["categories", "get"] [] `shouldBe`
            Right (WhoWhat Nothing $ ACategory $ Read $ Paginated defaultPage defaultSize $ GetCategories)

        it "correctly handles creating categories" $
            requestToAction1 ["categories", "create"] [("name", Just "catName1"), ("parent_id", Just "2")]
            `shouldBe` Right (WhoWhat Nothing $ ACategory $ Create $ CreateCategory {
                _cc_catName = "catName1",
                _cc_parentCat = 2
                })

        it "correctly handles editing categories" $
            requestToAction1 ["categories", "edit"] [("category_id", Just "4"), ("name", Just "catName1"), ("parent_id", Just "2")]
            `shouldBe` Right (WhoWhat Nothing $ ACategory $ Update $ EditCategory {
                _ec_catId = 4,
                _ec_catName = Just "catName1",
                _ec_parentId = Just 2
                })

        it "correctly handles deleting categories" $
            requestToAction1 ["categories", "delete"] [("category_id", Just "5")] `shouldBe`
            Right (WhoWhat Nothing $ ACategory $ Delete $ DeleteCategory {
                _dc_catId = 5
                })


testTagsRouting :: Spec
testTagsRouting = do
    describe "tags routing" $ do
        it "correctly handles getting tags" $
            requestToAction1 ["tags", "get"] [] `shouldBe`
            Right (WhoWhat Nothing $ ATags $ Read $ Paginated defaultPage defaultSize $ GetTags)

        it "correctly handles creating tags" $
            requestToAction1 ["tags", "create"] [("name", Just "tagName1")]
            `shouldBe` Right (WhoWhat Nothing $ ATags $ Create $ CreateTag {
                _ct_tagName = "tagName1"
                })

        it "correctly handles editing tag" $
            requestToAction1 ["tags", "edit"] [("tag_id", Just "4"), ("name", Just "tagName1")]
            `shouldBe` Right (WhoWhat Nothing $ ATags $ Update $ EditTag {
                _et_tagId = 4,
                _et_tagName = "tagName1"
                })

        it "correctly handles deleting tag" $
            requestToAction1 ["tags", "delete"] [("tag_id", Just "5")] `shouldBe`
            Right (WhoWhat Nothing $ ATags $ Delete $ DeleteTag {
                _dt_tagId = 5
                })




testUsersRouting :: Spec
testUsersRouting = do
    describe "users routing" $ do
        it "correctly handles getting profile" $
            requestToAction1 ["users", "profile"] [] `shouldBe`
            Right (WhoWhat Nothing $ AUsers $ Read GetProfile)

        it "correctly handles creating user" $
            requestToAction1 ["users", "create"]
            [("login", Just "login1"), ("pass_hash", Just "password"),
             ("firstname", Just "fn"), ("lastname", Just "ln")] `shouldBe`
            Right (WhoWhat Nothing $ AUsers $ Create $ CreateUser {
                _cu_login = "login1"
                , _cu_passHash = "password"
                , _cu_firstName = "fn"
                , _cu_lastName = "ln"
            })

        it "correctly handles deleting user" $
            requestToAction1 ["users", "delete"] [("user_id", Just "2")] `shouldBe`
            Right (WhoWhat Nothing $ AUsers $ Delete $ DeleteUser {
                _du_userId = 2
                })


testAuthorsRouting :: Spec
testAuthorsRouting = do
    describe "authors routing" $ do
        it "correctly handles getting authors" $
            requestToAction1 ["authors", "get"] [] `shouldBe`
            Right (WhoWhat Nothing $ AAuthors $ Read $ Paginated defaultPage defaultSize $ GetAuthors Nothing)

        it "correctly handles creating authors" $
            requestToAction1 ["authors", "create"] [("user_id", Just "2"), ("description", Just "description1")] `shouldBe`
            Right (WhoWhat Nothing $ AAuthors $ Create $ CreateAuthor {
                _ca_userId = 2
                , _ca_description = "description1"
                })


        it "correctly handles editing authors" $
            requestToAction1 ["authors", "edit"]
            [("author_id", Just "3"), ("user_id", Just "4"), ("description", Just "description1")]
            `shouldBe`
            Right (WhoWhat Nothing $ AAuthors $ Update $ EditAuthor {
                _ea_authorId = 3
                , _ea_userId = Just 4
                , _ea_description = Just "description1"
                })

        it "correctly handles delete author" $
            requestToAction1 ["authors", "delete"]
            [("author_id", Just "6")] `shouldBe` Right (WhoWhat Nothing $AAuthors $ Delete $ DeleteAuthor {
                _da_authorId = 6
                })

testCommentsRouting :: Spec
testCommentsRouting = do
    describe "comments routing" $ do
        it "correctly handles getting comments" $
            requestToAction1 ["comments", "get"] [("post_id", Just "123")] `shouldBe`
            Right (WhoWhat Nothing $AComments $ Read $ Paginated defaultPage defaultSize $ GetComments 123)

        it "correctly handles creating comment" $
            requestToAction1 ["comments", "create"] [("post_id", Just "123"), ("content", Just "content1")] `shouldBe`
            Right (WhoWhat Nothing $ AComments $ Create $ CreateComment {
                _ccom_postId = 123
                , _ccom_content = "content1"
                })

        it "correctly handles deleting comment" $
            requestToAction1 ["comments", "delete"] [("comment_id", Just "3")] `shouldBe`
            Right (WhoWhat Nothing $ AComments $ Delete $ DeleteComment {
                _dc_commentId = 3
                })


