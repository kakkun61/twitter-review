# Run following.
# mysql -u root < create-tables.sql

use `twitter-review`;

CREATE TABLE `user` (
  `id`           BIGINT UNSIGNED                   NOT NULL AUTO_INCREMENT,
  `email`        VARCHAR(254) CHARACTER SET latin1 NOT NULL,
  `display_name` TEXT,
  `token`        TEXT                              NOT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `unique_user` (`email`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;

CREATE TABLE `account` (
  `id`           BIGINT UNSIGNED NOT NULL,
  `screen_name`  TEXT            NOT NULL,
  `token`        TEXT            NOT NULL,
  `token_secret` TEXT            NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;

CREATE TABLE `user_account_relation` (
  `id`         BIGINT UNSIGNED  NOT NULL AUTO_INCREMENT,
  `user_id`    BIGINT UNSIGNED  NOT NULL,
  `account_id` BIGINT UNSIGNED  NOT NULL,
  `permission` TINYINT UNSIGNED NOT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `unique_user_account_relation` (`user_id`,`account_id`),
  KEY `user_account_relation_user_id_fkey` (`user_id`),
  KEY `user_account_relation_account_id_fkey` (`account_id`),
  CONSTRAINT `user_account_relation_account_id_fkey` FOREIGN KEY (`account_id`) REFERENCES `account` (`id`),
  CONSTRAINT `user_account_relation_user_id_fkey`    FOREIGN KEY (`user_id`)    REFERENCES `user` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;

CREATE TABLE `tweet` (
  `id`         BIGINT UNSIGNED   NOT NULL AUTO_INCREMENT,
  `account_id` BIGINT UNSIGNED   NOT NULL,
  `user_id`    BIGINT UNSIGNED   NOT NULL,
  `status`     TINYINT UNSIGNED  NOT NULL,
  `created`    DATETIME          NOT NULL,
  PRIMARY KEY (`id`),
  KEY `tweet_account_id_fkey` (`account_id`),
  KEY `tweet_user_id_fkey` (`user_id`),
  CONSTRAINT `tweet_account_id_fkey` FOREIGN KEY (`account_id`) REFERENCES `account` (`id`),
  CONSTRAINT `tweet_user_id_fkey` FOREIGN KEY (`user_id`) REFERENCES `user` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;

CREATE TABLE `tweet_uri` (
  `tweet_id` BIGINT UNSIGNED NOT NULL,
  `uri`      TEXT            NOT NULL,
  PRIMARY KEY (`tweet_id`),
  CONSTRAINT `tweet_uri_tweet_id_fkey` FOREIGN KEY (`tweet_id`) REFERENCES `tweet` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;

CREATE TABLE `tweet_candidate` (
  `id`       BIGINT UNSIGNED   NOT NULL AUTO_INCREMENT,
  `tweet_id` BIGINT UNSIGNED   NOT NULL,
  `TEXT`     TEXT              NOT NULL,
  `user_id`  BIGINT UNSIGNED   NOT NULL,
  `created`  DATETIME          NOT NULL,
  PRIMARY KEY (`id`),
  KEY `tweet_candidate_tweet_id_fkey` (`tweet_id`),
  CONSTRAINT `tweet_candidate_tweet_id_fkey` FOREIGN KEY (`tweet_id`) REFERENCES `tweet` (`id`),
  CONSTRAINT `tweet_candidate_user_id_fkey`  FOREIGN KEY (`user_id`)  REFERENCES `user` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;

CREATE TABLE `comment` (
  `id`       BIGINT UNSIGNED   NOT NULL AUTO_INCREMENT,
  `tweet_id` BIGINT UNSIGNED   NOT NULL,
  `TEXT`     TEXT              NOT NULL,
  `user`     BIGINT UNSIGNED   NOT NULL,
  `created`  DATETIME          NOT NULL,
  PRIMARY KEY (`id`),
  KEY `comment_tweet_id_fkey` (`tweet_id`),
  CONSTRAINT `comment_user_fkey` FOREIGN KEY (`user`) REFERENCES `user` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin
