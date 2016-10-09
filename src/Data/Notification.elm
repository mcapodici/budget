module Data.Notification exposing (..)

type alias NotificationSet =
  {
    notifications : List Notification,
    nextId : Int
  }

emptyNotificationSet : NotificationSet
emptyNotificationSet =
  { notifications = []
  , nextId = 0
  }

addNotification : String -> NotificationSet -> (NotificationSet, Notification)
addNotification message set =
  let newNotification =
    { message = message
    , id = set.nextId
    } in
  (
    { notifications = newNotification :: set.notifications
    , nextId = set.nextId + 1
    }
    , newNotification
  )

removeNotification : Notification -> NotificationSet -> NotificationSet
removeNotification n set =
  { set | notifications = List.filter (\n' -> n'.id /= n.id) set.notifications }

type alias Notification =
  { message : String
  , id : Int
  }
