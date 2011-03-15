def pair_13(nums) :
  for i in range(len(nums) - 1):
    if nums[i]==13 and nums[i+1]==13:
      return True

  return False ## if we get here, there was not a pair of 13's

