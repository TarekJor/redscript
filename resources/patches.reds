
/* multiply all crafting XP by 100 */
@replaceMethod(CraftingSystem)
private final func ProcessCraftSkill(xpAmount: Int32, craftedItem: StatsObjectID) {
  let xpEvent = new ExperiencePointsEvent();
  xpEvent.amount = xpAmount * 100;
  xpEvent.type = gamedataProficiencyType.Crafting;
  GetPlayer(this.GetGameInstance()).QueueEvent(xpEvent);
}

/* disable fake underwear when removing clothes */
@replaceMethod(EquipmentSystemPlayerData)
private final func EvaluateUnderwearVisibility(unequippedItem: ItemID) -> Bool {
  return false;
}

/* allow disassembling of all items */
@replaceMethod(CraftingSystem)
public final const func CanItemBeDisassembled(itemData: wref<gameItemData>) -> Bool {
  return true;
}

/* allow unequipping of all items */
@replaceMethod(RPGManager)
public final static func CanPartBeUnequipped(itemID: ItemID) -> Bool {
  return true;
}



/* sell edibles as junk automatically at vendors */
@replaceMethod(FullscreenVendorGameController)
private final func GetSellableJunk() -> array<wref<gameItemData>> {
  let result: array<wref<gameItemData>>;
  let sellableItems = this.m_VendorDataManager.GetItemsPlayerCanSell();
  let i: Uint32 = Cast(0);
  while i < Cast(ArraySize(sellableItems)) {
    let type = RPGManager.GetItemRecord(sellableItems[i].GetID()).ItemType().Type();
    if Equals(type, gamedataItemType.Gen_Junk) || Equals(type, gamedataItemType.Con_Edible) {
        let tmp: wref<gameItemData> = sellableItems[i];
        ArrayPush(result, tmp);
    };
    i += Cast(1);
  };
  return result;
}

/* switch cases fallthrough, break not supported yet (required by UpgradeItem patch) */
func CalculateCraftingExp(quality: gamedataQuality) -> Int32 {
    switch (quality) {
        case gamedataQuality.Common:
            return TweakDBInterface.GetInt(
                t"Constants.CraftingSystem.commonIngredientXP", 0);
        case gamedataQuality.Uncommon:
            return TweakDBInterface.GetInt(
                t"Constants.CraftingSystem.uncommonIngredientXP", 0);
        case gamedataQuality.Rare:
            return TweakDBInterface.GetInt(
                t"Constants.CraftingSystem.rareIngredientXP", 0);
        case gamedataQuality.Epic:
            return TweakDBInterface.GetInt(
                t"Constants.CraftingSystem.epicIngredientXP", 0);
        case gamedataQuality.Legendary:
            return TweakDBInterface.GetInt(
                t"Constants.CraftingSystem.legendaryIngredientXP", 0);
        default:
            return TweakDBInterface.GetInt(
                t"Constants.CraftingSystem.commonIngredientXP", 0);
    }
}

/* updates items right up to current player level, reduces overall upgrade cost */
@replaceMethod(CraftingSystem)
private final func UpgradeItem(owner: wref<GameObject>, itemID: ItemID) {
    let recipeXP: Int32 = 0;
    let randF = RandF();
    let statsSystem = GameInstance.GetStatsSystem(this.GetGameInstance());
    let TS = GameInstance.GetTransactionSystem(this.GetGameInstance());
    let itemData = TS.GetItemData(owner, itemID);
    let statsObjectId = itemData.GetStatsObjectID();
    let materialRetrieveChance = statsSystem.GetStatValue(
        Cast(owner.GetEntityID()),
        gamedataStatType.UpgradingMaterialRetrieveChance);
    let ingredients = this.GetItemFinalUpgradeCost(itemData);
    let i = 0;
    while i < ArraySize(ingredients) {
        if randF >= materialRetrieveChance {
            TS.RemoveItem(owner, ItemID.CreateQuery(ingredients[i].id.GetID()),
                ingredients[i].quantity);
        }
        let ingredientQuality = RPGManager.GetItemQualityFromRecord(
            TweakDBInterface.GetItemRecord(ingredients[i].id.GetID()));
        recipeXP += CalculateCraftingExp(ingredientQuality) * ingredients[i].quantity;
        i += 1;
    }
    let previousItemUpgrade = itemData.GetStatValueByType(gamedataStatType.WasItemUpgraded);
    let itemLevel = itemData.GetStatValueByType(gamedataStatType.ItemLevel) / 10.0;
    let playerPowerLevel = statsSystem.GetStatValue(Cast(owner.GetEntityID()), gamedataStatType.PowerLevel);
    let newItemUpgrade: Float = Cast(CeilF(previousItemUpgrade + (playerPowerLevel - itemLevel)));

    statsSystem.RemoveAllModifiers(statsObjectId, gamedataStatType.WasItemUpgraded, true);
    let mod = RPGManager.CreateStatModifier(
        gamedataStatType.WasItemUpgraded, gameStatModifierType.Additive, newItemUpgrade);
    statsSystem.AddSavedModifier(statsObjectId, mod);
    this.ProcessCraftSkill(recipeXP, statsObjectId);
}
