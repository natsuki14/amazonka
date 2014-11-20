{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SQS.ReceiveMessage
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Retrieves one or more messages, with a maximum limit of 10 messages, from
-- the specified queue. Long poll support is enabled by using the
-- WaitTimeSeconds parameter. For more information, see Amazon SQS Long Poll
-- in the Amazon SQS Developer Guide. Short poll is the default behavior where
-- a weighted random set of machines is sampled on a ReceiveMessage call. This
-- means only the messages on the sampled machines are returned. If the number
-- of messages in the queue is small (less than 1000), it is likely you will
-- get fewer messages than you requested per ReceiveMessage call. If the
-- number of messages in the queue is extremely small, you might not receive
-- any messages in a particular ReceiveMessage response; in which case you
-- should repeat the request. For each message returned, the response includes
-- the following: Message body MD5 digest of the message body. For information
-- about MD5, go to http://www.faqs.org/rfcs/rfc1321.html. Message ID you
-- received when you sent the message to the queue. Receipt handle. Message
-- attributes. MD5 digest of the message attributes. The receipt handle is the
-- identifier you must provide when deleting the message. For more
-- information, see Queue and Message Identifiers in the Amazon SQS Developer
-- Guide. You can provide the VisibilityTimeout parameter in your request,
-- which will be applied to the messages that Amazon SQS returns in the
-- response. If you do not include the parameter, the overall visibility
-- timeout for the queue is used for the returned messages. For more
-- information, see Visibility Timeout in the Amazon SQS Developer Guide.
--
-- <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/APIReference/API_ReceiveMessage.html>
module Network.AWS.SQS.ReceiveMessage
    (
    -- * Request
      ReceiveMessage
    -- ** Request constructor
    , receiveMessage
    -- ** Request lenses
    , rmAttributeNames
    , rmMaxNumberOfMessages
    , rmMessageAttributeNames
    , rmQueueUrl
    , rmVisibilityTimeout
    , rmWaitTimeSeconds

    -- * Response
    , ReceiveMessageResponse
    -- ** Response constructor
    , receiveMessageResponse
    -- ** Response lenses
    , rmrMessages
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.SQS.Types
import qualified GHC.Exts

data ReceiveMessage = ReceiveMessage
    { _rmAttributeNames        :: List "AttributeName" Text
    , _rmMaxNumberOfMessages   :: Maybe Int
    , _rmMessageAttributeNames :: List "MessageAttributeName" Text
    , _rmQueueUrl              :: Text
    , _rmVisibilityTimeout     :: Maybe Int
    , _rmWaitTimeSeconds       :: Maybe Int
    } deriving (Eq, Ord, Show)

-- | 'ReceiveMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rmAttributeNames' @::@ ['Text']
--
-- * 'rmMaxNumberOfMessages' @::@ 'Maybe' 'Int'
--
-- * 'rmMessageAttributeNames' @::@ ['Text']
--
-- * 'rmQueueUrl' @::@ 'Text'
--
-- * 'rmVisibilityTimeout' @::@ 'Maybe' 'Int'
--
-- * 'rmWaitTimeSeconds' @::@ 'Maybe' 'Int'
--
receiveMessage :: Text -- ^ 'rmQueueUrl'
               -> [Text] -- ^ 'rmAttributeNames'
               -> [Text] -- ^ 'rmMessageAttributeNames'
               -> ReceiveMessage
receiveMessage p1 p2 p3 = ReceiveMessage
    { _rmQueueUrl              = p1
    , _rmAttributeNames        = withIso _List (const id) p2
    , _rmMessageAttributeNames = withIso _List (const id) p3
    , _rmMaxNumberOfMessages   = Nothing
    , _rmVisibilityTimeout     = Nothing
    , _rmWaitTimeSeconds       = Nothing
    }

-- | A list of attributes that need to be returned along with each message.
-- The following lists the names and descriptions of the attributes that can
-- be returned: All - returns all values. ApproximateFirstReceiveTimestamp -
-- returns the time when the message was first received (epoch time in
-- milliseconds). ApproximateReceiveCount - returns the number of times a
-- message has been received but not deleted. SenderId - returns the AWS
-- account number (or the IP address, if anonymous access is allowed) of the
-- sender. SentTimestamp - returns the time when the message was sent (epoch
-- time in milliseconds).
rmAttributeNames :: Lens' ReceiveMessage [Text]
rmAttributeNames = lens _rmAttributeNames (\s a -> s { _rmAttributeNames = a }) . _List

-- | The maximum number of messages to return. Amazon SQS never returns more
-- messages than this value but may return fewer. Values can be from 1 to
-- 10. Default is 1. All of the messages are not necessarily returned.
rmMaxNumberOfMessages :: Lens' ReceiveMessage (Maybe Int)
rmMaxNumberOfMessages =
    lens _rmMaxNumberOfMessages (\s a -> s { _rmMaxNumberOfMessages = a })

-- | The message attribute Name can contain the following characters: A-Z,
-- a-z, 0-9, underscore(_), hyphen(-), and period (.). The message attribute
-- name must not start or end with a period, and it should not have
-- successive periods. The message attribute name is case sensitive and must
-- be unique among all attribute names for the message. The message
-- attribute name can be up to 256 characters long. Attribute names cannot
-- start with "AWS." or "Amazon." because these prefixes are reserved for
-- use by Amazon Web Services.
rmMessageAttributeNames :: Lens' ReceiveMessage [Text]
rmMessageAttributeNames =
    lens _rmMessageAttributeNames (\s a -> s { _rmMessageAttributeNames = a })
        . _List

-- | The URL of the Amazon SQS queue to take action on.
rmQueueUrl :: Lens' ReceiveMessage Text
rmQueueUrl = lens _rmQueueUrl (\s a -> s { _rmQueueUrl = a })

-- | The duration (in seconds) that the received messages are hidden from
-- subsequent retrieve requests after being retrieved by a ReceiveMessage
-- request.
rmVisibilityTimeout :: Lens' ReceiveMessage (Maybe Int)
rmVisibilityTimeout =
    lens _rmVisibilityTimeout (\s a -> s { _rmVisibilityTimeout = a })

-- | The duration (in seconds) for which the call will wait for a message to
-- arrive in the queue before returning. If a message is available, the call
-- will return sooner than WaitTimeSeconds.
rmWaitTimeSeconds :: Lens' ReceiveMessage (Maybe Int)
rmWaitTimeSeconds =
    lens _rmWaitTimeSeconds (\s a -> s { _rmWaitTimeSeconds = a })

newtype ReceiveMessageResponse = ReceiveMessageResponse
    { _rmrMessages :: List "Message" Message
    } deriving (Eq, Show, Monoid, Semigroup)

-- | 'ReceiveMessageResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rmrMessages' @::@ ['Message']
--
receiveMessageResponse :: [Message] -- ^ 'rmrMessages'
                       -> ReceiveMessageResponse
receiveMessageResponse p1 = ReceiveMessageResponse
    { _rmrMessages = withIso _List (const id) p1
    }

-- | A list of messages.
rmrMessages :: Lens' ReceiveMessageResponse [Message]
rmrMessages = lens _rmrMessages (\s a -> s { _rmrMessages = a }) . _List

instance ToPath ReceiveMessage where
    toPath = const "/"

instance ToQuery ReceiveMessage where
    toQuery ReceiveMessage{..} = mconcat
        [ toQuery                _rmAttributeNames
        , "MaxNumberOfMessages"   =? _rmMaxNumberOfMessages
        , toQuery                _rmMessageAttributeNames
        , "QueueUrl"              =? _rmQueueUrl
        , "VisibilityTimeout"     =? _rmVisibilityTimeout
        , "WaitTimeSeconds"       =? _rmWaitTimeSeconds
        ]

instance ToHeaders ReceiveMessage

instance AWSRequest ReceiveMessage where
    type Sv ReceiveMessage = SQS
    type Rs ReceiveMessage = ReceiveMessageResponse

    request  = post "ReceiveMessage"
    response = xmlResponse

instance FromXML ReceiveMessageResponse where
    parseXML = withElement "ReceiveMessageResult" $ \x -> ReceiveMessageResponse
        <$> parseXML x


Some kind of operator / class to check the types whether to continue?
