;; Cryptoeconomic Primitive Smart Contract
;; This contract implements various cryptoeconomic primitives for DeFi applications

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INSUFFICIENT-BALANCE (err u101))
(define-constant ERR-INVALID-PARAMETER (err u102))
(define-constant ERR-POOL-DEPLETED (err u103))
(define-constant ERR-OWNER-ONLY (err u104))
(define-constant ERR-NOT-ACTIVE (err u105))
(define-constant ERR-ALREADY-INITIALIZED (err u106))
(define-constant ERR-EXPIRED (err u107))

;; Contract variables
(define-data-var contract-owner principal tx-sender)
(define-data-var protocol-fee-percent uint u5) ;; 0.5% default fee
(define-data-var is-active bool true)
(define-data-var total-liquidity uint u0)
(define-data-var last-price uint u0)
(define-data-var contract-initialized bool false)

;; Asset maps
(define-map user-balances {user: principal, token-id: uint} {amount: uint})
(define-map liquidity-providers principal {amount: uint, last-deposit-block: uint})
(define-map token-pools uint {reserve: uint, weight: uint})
(define-map bonding-curves uint {type: (string-ascii 20), params: (list 5 uint)})
(define-map staking-positions
  {user: principal, pool-id: uint}
  {amount: uint, rewards: uint, start-block: uint, end-block: uint})

;; Constants
(define-constant PRECISION_FACTOR u1000000) ;; 6 decimal places of precision
(define-constant MIN_LIQUIDITY u1000) ;; Minimum liquidity to start
(define-constant MAX_WEIGHT u1000000) ;; Maximum weight for a token (100%)
(define-constant BLOCKS_PER_YEAR u52560) ;; ~365 days with 10-minute blocks

;; Helper functions

;; Non-recursive power function (supports limited exponents)
(define-private (pow-uint (base uint) (exp uint))
  (if (> exp u10) 
      u0  ;; Return 0 for exponents that are too large
      (if (is-eq exp u0) 
          u1
          (if (is-eq exp u1)
              base
              (if (is-eq exp u2)
                  (* base base)
                  (if (is-eq exp u3)
                      (* base (* base base))
                      (if (is-eq exp u4)
                          (* base (* base (* base base)))
                          (if (is-eq exp u5)
                              (* base (* base (* base (* base base))))
                              (if (is-eq exp u6)
                                  (* base (* base (* base (* base (* base base)))))
                                  (if (is-eq exp u7)
                                      (* base (* base (* base (* base (* base (* base base))))))
                                      (if (is-eq exp u8)
                                          (* base (* base (* base (* base (* base (* base (* base base)))))))
                                          (if (is-eq exp u9)
                                              (* base (* base (* base (* base (* base (* base (* base (* base base))))))))
                                              (* base (* base (* base (* base (* base (* base (* base (* base (* base base)))))))))
                                          )
                                      )
                                  )
                              )
                          )
                      )
                  )
              )
          )
      )
  ))

;; Read-only functions

;; Get balance for a specific token
(define-read-only (get-balance (user principal) (token-id uint))
  (default-to u0 (get amount (map-get? user-balances {user: user, token-id: token-id}))))

;; Check if the caller is the contract owner
(define-read-only (is-owner)
  (is-eq tx-sender (var-get contract-owner)))

;; Get token pool information
(define-read-only (get-pool-info (token-id uint))
  (map-get? token-pools token-id))

;; Get current protocol fee
(define-read-only (get-protocol-fee)
  (var-get protocol-fee-percent))

;; Get contract status
(define-read-only (get-contract-status)
  (var-get is-active))

;; Get total liquidity in the contract
(define-read-only (get-total-liquidity)
  (var-get total-liquidity))

;; Get liquidity provider information
(define-read-only (get-provider-info (provider principal))
  (map-get? liquidity-providers provider))

;; Get bonding curve for a token
(define-read-only (get-bonding-curve (token-id uint))
  (map-get? bonding-curves token-id))

;; Get staking position for a user
(define-read-only (get-staking-position (user principal) (pool-id uint))
  (map-get? staking-positions {user: user, pool-id: pool-id}))

;; Calculate price using the bonding curve
(define-read-only (calculate-price (token-id uint) (amount uint))
  (match (map-get? bonding-curves token-id)
    curve
    (let (
      (curve-type (get type curve))
      (curve-params (get params curve))
      (current-supply (default-to u0 (get reserve (map-get? token-pools token-id))))
    )
    (if (is-eq curve-type "linear")
        ;; Linear: price = m * supply + b
        ;; params[0] = m (slope), params[1] = b (y-intercept)
        (+ (* (default-to u0 (element-at? curve-params u0)) current-supply) 
           (default-to u0 (element-at? curve-params u1)))
        
        (if (is-eq curve-type "exponential")
            ;; Exponential: price = a * (b ^ supply)
            ;; params[0] = a, params[1] = b (scaled by PRECISION_FACTOR)
            (let (
              (a (default-to u0 (element-at? curve-params u0)))
              (b (default-to u0 (element-at? curve-params u1)))
              (scaled-exp (/ (* b current-supply) PRECISION_FACTOR))
            )
            (* a (pow-uint u2 scaled-exp))) ;; Using base 2 with scaled exponent for simplicity
            
            ;; Constant: price is fixed
            ;; params[0] = fixed price
            (default-to u0 (element-at? curve-params u0))
        )
    ))
    ;; Return default value if bonding curve not found
    u0
  ))

;; Calculate rewards for staking
(define-read-only (calculate-staking-rewards (user principal) (pool-id uint))
  (match (map-get? staking-positions {user: user, pool-id: pool-id})
    position
    (let (
      (staked-amount (get amount position))
      (start-block (get start-block position))
      (current-rewards (get rewards position))
      (blocks-staked (- block-height start-block))
      (reward-rate (/ (* staked-amount blocks-staked) BLOCKS_PER_YEAR))
    )
    (+ current-rewards reward-rate))
    ;; Return 0 if no staking position found
    u0
  ))

;; Calculate weighted price for a swap
(define-read-only (calculate-swap-price (token-in uint) (token-out uint) (amount-in uint))
  (match (map-get? token-pools token-in)
    pool-in
    (match (map-get? token-pools token-out)
      pool-out
      (let (
        (reserve-in (get reserve pool-in))
        (reserve-out (get reserve pool-out))
        (weight-in (get weight pool-in))
        (weight-out (get weight pool-out))
        (fee (var-get protocol-fee-percent))
        (fee-amount (/ (* amount-in fee) u1000)) ;; fee is in 0.1% units
        (amount-in-after-fee (- amount-in fee-amount))
        (price-ratio (/ (* reserve-out weight-in) (* reserve-in weight-out)))
      )
      (/ (* amount-in-after-fee price-ratio) PRECISION_FACTOR))
      ;; Return 0 if output pool not found
      u0
    )
    ;; Return 0 if input pool not found
    u0
  ))

;; Public functions

;; Initialize the contract
(define-public (initialize (owner principal))
  (begin
    (asserts! (not (var-get contract-initialized)) ERR-ALREADY-INITIALIZED)
    (var-set contract-owner owner)
    (var-set contract-initialized true)
    (ok true)))

;; Update contract owner (owner only)
(define-public (set-owner (new-owner principal))
  (begin
    (asserts! (is-owner) ERR-OWNER-ONLY)
    (var-set contract-owner new-owner)
    (ok true)))

;; Update protocol fee (owner only)
(define-public (set-protocol-fee (new-fee uint))
  (begin
    (asserts! (is-owner) ERR-OWNER-ONLY)
    ;; Fee is in 0.1% units, max 5% (50)
    (asserts! (<= new-fee u50) ERR-INVALID-PARAMETER)
    (var-set protocol-fee-percent new-fee)
    (ok true)))

;; Activate/deactivate contract (owner only)
(define-public (set-contract-status (active bool))
  (begin
    (asserts! (is-owner) ERR-OWNER-ONLY)
    (var-set is-active active)
    (ok true)))

;; Create a new token pool
(define-public (create-pool (token-id uint) (initial-reserve uint) (weight uint))
  (begin
    (asserts! (is-owner) ERR-OWNER-ONLY)
    (asserts! (var-get is-active) ERR-NOT-ACTIVE)
    (asserts! (<= weight MAX_WEIGHT) ERR-INVALID-PARAMETER)
    (asserts! (is-none (map-get? token-pools token-id)) ERR-INVALID-PARAMETER)
    (map-set token-pools token-id {reserve: initial-reserve, weight: weight})
    (var-set total-liquidity (+ (var-get total-liquidity) initial-reserve))
    (ok true)))

;; Set bonding curve for a token
(define-public (set-bonding-curve (token-id uint) (curve-type (string-ascii 20)) (params (list 5 uint)))
  (begin
    (asserts! (is-owner) ERR-OWNER-ONLY)
    (asserts! (var-get is-active) ERR-NOT-ACTIVE)
    (asserts! (or (is-eq curve-type "linear") (is-eq curve-type "exponential") (is-eq curve-type "constant")) ERR-INVALID-PARAMETER)
    (map-set bonding-curves token-id {type: curve-type, params: params})
    (ok true)))

;; Add liquidity to a pool
(define-public (add-liquidity (token-id uint) (amount uint))
  (begin
    (asserts! (var-get is-active) ERR-NOT-ACTIVE)
    (asserts! (> amount u0) ERR-INVALID-PARAMETER)
    
    ;; Get current pool
    (let (
      (pool (unwrap! (map-get? token-pools token-id) ERR-INVALID-PARAMETER))
      (current-reserve (get reserve pool))
      (provider-info (default-to {amount: u0, last-deposit-block: u0} 
                     (map-get? liquidity-providers tx-sender)))
      (provider-amount (get amount provider-info))
    )
    
    ;; Update pool
    (map-set token-pools token-id {
      reserve: (+ current-reserve amount),
      weight: (get weight pool)
    })
    
    ;; Update provider data
    (map-set liquidity-providers tx-sender {
      amount: (+ provider-amount amount),
      last-deposit-block: block-height
    })
    
    ;; Update total liquidity
    (var-set total-liquidity (+ (var-get total-liquidity) amount))
    
    (ok true))))

;; Remove liquidity from a pool
(define-public (remove-liquidity (token-id uint) (amount uint))
  (begin
    (asserts! (var-get is-active) ERR-NOT-ACTIVE)
    
    ;; Get current pool and provider data
    (let (
      (pool (unwrap! (map-get? token-pools token-id) ERR-INVALID-PARAMETER))
      (current-reserve (get reserve pool))
      (provider-info (unwrap! (map-get? liquidity-providers tx-sender) ERR-INSUFFICIENT-BALANCE))
      (provider-amount (get amount provider-info))
    )
    
    ;; Check balance
    (asserts! (>= provider-amount amount) ERR-INSUFFICIENT-BALANCE)
    (asserts! (>= current-reserve amount) ERR-POOL-DEPLETED)
    
    ;; Update pool
    (map-set token-pools token-id {
      reserve: (- current-reserve amount),
      weight: (get weight pool)
    })
    
    ;; Update provider data
    (map-set liquidity-providers tx-sender {
      amount: (- provider-amount amount),
      last-deposit-block: (get last-deposit-block provider-info)
    })
    
    ;; Update total liquidity
    (var-set total-liquidity (- (var-get total-liquidity) amount))
    
    (ok true))))

;; Deposit tokens into user balance
(define-public (deposit (token-id uint) (amount uint))
  (begin
    (asserts! (var-get is-active) ERR-NOT-ACTIVE)
    (asserts! (> amount u0) ERR-INVALID-PARAMETER)
    
    (let (
      (current-balance (get-balance tx-sender token-id))
    )
    
    (map-set user-balances 
      {user: tx-sender, token-id: token-id}
      {amount: (+ current-balance amount)})
    
    (ok true))))

;; Withdraw tokens from user balance
(define-public (withdraw (token-id uint) (amount uint))
  (begin
    (asserts! (var-get is-active) ERR-NOT-ACTIVE)
    
    (let (
      (current-balance (get-balance tx-sender token-id))
    )
    
    (asserts! (>= current-balance amount) ERR-INSUFFICIENT-BALANCE)
    
    (map-set user-balances 
      {user: tx-sender, token-id: token-id}
      {amount: (- current-balance amount)})
    
    (ok true))))

;; Swap tokens between pools
(define-public (swap (token-in uint) (token-out uint) (amount-in uint))
  (begin
    (asserts! (var-get is-active) ERR-NOT-ACTIVE)
    (asserts! (> amount-in u0) ERR-INVALID-PARAMETER)
    (asserts! (not (is-eq token-in token-out)) ERR-INVALID-PARAMETER)
    
    (let (
      (user-balance-in (get-balance tx-sender token-in))
      (pool-in (unwrap! (map-get? token-pools token-in) ERR-INVALID-PARAMETER))
      (pool-out (unwrap! (map-get? token-pools token-out) ERR-INVALID-PARAMETER))
      (reserve-in (get reserve pool-in))
      (reserve-out (get reserve pool-out))
      (amount-out (calculate-swap-price token-in token-out amount-in))
    )
    
    ;; Check balances
    (asserts! (>= user-balance-in amount-in) ERR-INSUFFICIENT-BALANCE)
    (asserts! (>= reserve-out amount-out) ERR-POOL-DEPLETED)
    
    ;; Update user balances
    (map-set user-balances 
      {user: tx-sender, token-id: token-in}
      {amount: (- user-balance-in amount-in)})
    
    (map-set user-balances 
      {user: tx-sender, token-id: token-out}
      {amount: (+ (get-balance tx-sender token-out) amount-out)})
    
    ;; Update pools
    (map-set token-pools token-in {
      reserve: (+ reserve-in amount-in),
      weight: (get weight pool-in)
    })
    
    (map-set token-pools token-out {
      reserve: (- reserve-out amount-out),
      weight: (get weight pool-out)
    })
    
    ;; Update last price
    (var-set last-price (/ (* amount-out PRECISION_FACTOR) amount-in))
    
    (ok amount-out))))

;; Stake tokens in a staking pool
(define-public (stake (pool-id uint) (amount uint) (lock-blocks uint))
  (begin
    (asserts! (var-get is-active) ERR-NOT-ACTIVE)
    (asserts! (> amount u0) ERR-INVALID-PARAMETER)
    
    (let (
      (user-balance (get-balance tx-sender pool-id))
      (existing-position (map-get? staking-positions {user: tx-sender, pool-id: pool-id}))
    )
    
    ;; Check balance
    (asserts! (>= user-balance amount) ERR-INSUFFICIENT-BALANCE)
    
    ;; Decrease user balance
    (map-set user-balances 
      {user: tx-sender, token-id: pool-id}
      {amount: (- user-balance amount)})
    
    ;; Update or create staking position
    (if (is-some existing-position)
        (let (
          (position (unwrap-panic existing-position))
          (current-amount (get amount position))
          (current-rewards (get rewards position))
          (current-end-block (get end-block position))
          (new-end-block (+ block-height lock-blocks))
        )
        (map-set staking-positions
          {user: tx-sender, pool-id: pool-id}
          {
            amount: (+ current-amount amount),
            rewards: current-rewards,
            start-block: (get start-block position),
            end-block: (if (> new-end-block current-end-block) new-end-block current-end-block)
          }))
        
        ;; Create new position
        (map-set staking-positions
          {user: tx-sender, pool-id: pool-id}
          {
            amount: amount,
            rewards: u0,
            start-block: block-height,
            end-block: (+ block-height lock-blocks)
          }))
    
    (ok true))))

;; Unstake tokens from a staking pool
(define-public (unstake (pool-id uint))
  (begin
    (asserts! (var-get is-active) ERR-NOT-ACTIVE)
    
    (let (
      (position (unwrap! (map-get? staking-positions {user: tx-sender, pool-id: pool-id}) ERR-INVALID-PARAMETER))
      (staked-amount (get amount position))
      (current-rewards (get rewards position))
      (end-block (get end-block position))
    )
    
    ;; Check if lock period is over
    (asserts! (>= block-height end-block) ERR-EXPIRED)
    
    ;; Calculate final rewards
    (let (
      (final-rewards (calculate-staking-rewards tx-sender pool-id))
    )
    
    ;; Return tokens to user
    (map-set user-balances 
      {user: tx-sender, token-id: pool-id}
      {amount: (+ (get-balance tx-sender pool-id) staked-amount)})
    
    ;; Return rewards to user
    (map-set user-balances 
      {user: tx-sender, token-id: u0} ;; Rewards token
      {amount: (+ (get-balance tx-sender u0) final-rewards)})
    
    ;; Delete staking position
    (map-delete staking-positions {user: tx-sender, pool-id: pool-id})
    
    (ok final-rewards)))))

;; Claim rewards without unstaking
(define-public (claim-rewards (pool-id uint))
  (begin
    (asserts! (var-get is-active) ERR-NOT-ACTIVE)
    
    (let (
      (position (unwrap! (map-get? staking-positions {user: tx-sender, pool-id: pool-id}) ERR-INVALID-PARAMETER))
      (current-rewards (calculate-staking-rewards tx-sender pool-id))
    )
    
    ;; Update user balance with rewards
    (map-set user-balances 
      {user: tx-sender, token-id: u0} ;; Rewards token
      {amount: (+ (get-balance tx-sender u0) current-rewards)})
    
    ;; Reset rewards in staking position
    (map-set staking-positions
      {user: tx-sender, pool-id: pool-id}
      {
        amount: (get amount position),
        rewards: u0,
        start-block: block-height, ;; Reset start block for future reward calculation
        end-block: (get end-block position)
      })
    
    (ok current-rewards))))

;; Emergency functions

;; Emergency withdraw (owner only)
(define-public (emergency-withdraw (token-id uint) (amount uint) (recipient principal))
  (begin
    (asserts! (is-owner) ERR-OWNER-ONLY)
    
    (let (
      (pool (unwrap! (map-get? token-pools token-id) ERR-INVALID-PARAMETER))
      (current-reserve (get reserve pool))
    )
    
    (asserts! (>= current-reserve amount) ERR-POOL-DEPLETED)
    
    ;; Update pool
    (map-set token-pools token-id {
      reserve: (- current-reserve amount),
      weight: (get weight pool)
    })
    
    ;; Send to recipient
    (map-set user-balances 
      {user: recipient, token-id: token-id}
      {amount: (+ (get-balance recipient token-id) amount)})
    
    ;; Update total liquidity
    (var-set total-liquidity (- (var-get total-liquidity) amount))
    
    (ok true))))