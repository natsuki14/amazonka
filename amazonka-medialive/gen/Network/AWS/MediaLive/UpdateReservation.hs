{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.UpdateReservation
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update reservation.
module Network.AWS.MediaLive.UpdateReservation
    (
    -- * Creating a Request
      updateReservation
    , UpdateReservation
    -- * Request Lenses
    , urReservationId

    -- * Destructuring the Response
    , updateReservationResponse
    , UpdateReservationResponse
    -- * Response Lenses
    , urrsReservation
    , urrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types
import Network.AWS.MediaLive.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Request to update a reservation
--
-- /See:/ 'updateReservation' smart constructor.
newtype UpdateReservation = UpdateReservation'{_urReservationId
                                               :: Text}
                              deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateReservation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'urReservationId' - Unique reservation ID, e.g. '1234567'
updateReservation
    :: Text -- ^ 'urReservationId'
    -> UpdateReservation
updateReservation pReservationId_
  = UpdateReservation'{_urReservationId =
                         pReservationId_}

-- | Unique reservation ID, e.g. '1234567'
urReservationId :: Lens' UpdateReservation Text
urReservationId = lens _urReservationId (\ s a -> s{_urReservationId = a})

instance AWSRequest UpdateReservation where
        type Rs UpdateReservation = UpdateReservationResponse
        request = putJSON mediaLive
        response
          = receiveJSON
              (\ s h x ->
                 UpdateReservationResponse' <$>
                   (x .?> "reservation") <*> (pure (fromEnum s)))

instance Hashable UpdateReservation where

instance NFData UpdateReservation where

instance ToHeaders UpdateReservation where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateReservation where
        toJSON = const (Object mempty)

instance ToPath UpdateReservation where
        toPath UpdateReservation'{..}
          = mconcat
              ["/prod/reservations/", toBS _urReservationId]

instance ToQuery UpdateReservation where
        toQuery = const mempty

-- | Placeholder documentation for UpdateReservationResponse
--
-- /See:/ 'updateReservationResponse' smart constructor.
data UpdateReservationResponse = UpdateReservationResponse'{_urrsReservation
                                                            ::
                                                            !(Maybe
                                                                Reservation),
                                                            _urrsResponseStatus
                                                            :: !Int}
                                   deriving (Eq, Read, Show, Data, Typeable,
                                             Generic)

-- | Creates a value of 'UpdateReservationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'urrsReservation' - Undocumented member.
--
-- * 'urrsResponseStatus' - -- | The response status code.
updateReservationResponse
    :: Int -- ^ 'urrsResponseStatus'
    -> UpdateReservationResponse
updateReservationResponse pResponseStatus_
  = UpdateReservationResponse'{_urrsReservation =
                                 Nothing,
                               _urrsResponseStatus = pResponseStatus_}

-- | Undocumented member.
urrsReservation :: Lens' UpdateReservationResponse (Maybe Reservation)
urrsReservation = lens _urrsReservation (\ s a -> s{_urrsReservation = a})

-- | -- | The response status code.
urrsResponseStatus :: Lens' UpdateReservationResponse Int
urrsResponseStatus = lens _urrsResponseStatus (\ s a -> s{_urrsResponseStatus = a})

instance NFData UpdateReservationResponse where
