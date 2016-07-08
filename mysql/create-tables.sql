# Run following.
# mysql -u root < create-tables.sql

use `twitter-review`;

CREATE TABLE `user` (
  `id`           bigint(20)                        NOT NULL AUTO_INCREMENT,
  `email`        varchar(254) CHARACTER SET latin1 NOT NULL,
  `display_name` text,
  `token`        text                              NOT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `unique_user` (`email`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;

CREATE TABLE `account` (
  `id`           bigint(20) NOT NULL,
  `screen_name`  text       NOT NULL,
  `token`        text       NOT NULL,
  `token_secret` text       NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;

CREATE TABLE `user_account_relation` (
  `id`         bigint(20) NOT NULL AUTO_INCREMENT,
  `user_id`    bigint(20) NOT NULL,
  `account_id` bigint(20) NOT NULL,
  `permission` int(11)    NOT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `unique_user_account_relation` (`user_id`,`account_id`),
  KEY `user_account_relation_user_id_fkey` (`user_id`),
  KEY `user_account_relation_account_id_fkey` (`account_id`),
  CONSTRAINT `user_account_relation_account_id_fkey` FOREIGN KEY (`account_id`) REFERENCES `account` (`id`),
  CONSTRAINT `user_account_relation_user_id_fkey`    FOREIGN KEY (`user_id`)    REFERENCES `user` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;

CREATE TABLE `tweet` (
  `id`         bigint(20) NOT NULL AUTO_INCREMENT,
  `account_id` bigint(20) NOT NULL,
  `user_id`    bigint(20) NOT NULL,
  `status`     text       NOT NULL,
  `created`    datetime   NOT NULL,
  PRIMARY KEY (`id`),
  KEY `tweet_account_id_fkey` (`account_id`),
  KEY `tweet_user_id_fkey` (`user_id`),
  CONSTRAINT `tweet_account_id_fkey` FOREIGN KEY (`account_id`) REFERENCES `account` (`id`),
  CONSTRAINT `tweet_user_id_fkey` FOREIGN KEY (`user_id`) REFERENCES `user` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;

CREATE TABLE `tweet_candidate` (
  `id`       bigint(20) NOT NULL AUTO_INCREMENT,
  `tweet_id` bigint(20) NOT NULL,
  `text`     text       NOT NULL,
  `user_id`  bigint(20) NOT NULL,
  `created`  datetime   NOT NULL,
  PRIMARY KEY (`id`),
  KEY `tweet_candidate_tweet_id_fkey` (`tweet_id`),
  CONSTRAINT `tweet_candidate_tweet_id_fkey` FOREIGN KEY (`tweet_id`) REFERENCES `tweet` (`id`),
  CONSTRAINT `tweet_candidate_user_id_fkey`  FOREIGN KEY (`user_id`)  REFERENCES `user` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;

CREATE TABLE `comment` (
  `id`       bigint(20) NOT NULL AUTO_INCREMENT,
  `tweet_id` bigint(20) NOT NULL,
  `text`     text       NOT NULL,
  `user`     bigint(20) NOT NULL,
  `created`  datetime   NOT NULL,
  PRIMARY KEY (`id`),
  KEY `comment_tweet_id_fkey` (`tweet_id`),
  CONSTRAINT `comment_user_fkey` FOREIGN KEY (`user`) REFERENCES `user` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin
