package com.intellij.entities

import com.intellij.platform.workspace.jps.entities.ModuleEntity
import com.intellij.platform.workspace.storage.EntitySource
import com.intellij.platform.workspace.storage.EntityType
import com.intellij.platform.workspace.storage.GeneratedCodeApiVersion
import com.intellij.platform.workspace.storage.WorkspaceEntity
import com.intellij.platform.workspace.storage.annotations.Abstract
import com.intellij.platform.workspace.storage.annotations.Child

/**
 * Abstract entity that other entities that need to have extension property to ModuleEntity can inherit from.
 */
@Abstract
interface ModuleExtensionWorkspaceEntity: WorkspaceEntity {
    val module: ModuleEntity

    //region generated code
    @GeneratedCodeApiVersion(3)
    interface Builder<T : ModuleExtensionWorkspaceEntity> : WorkspaceEntity.Builder<T> {
        override var entitySource: EntitySource
        var module: ModuleEntity.Builder
    }

    companion object : EntityType<ModuleExtensionWorkspaceEntity, Builder<ModuleExtensionWorkspaceEntity>>() {
        @JvmOverloads
        @JvmStatic
        @JvmName("create")
        operator fun invoke(
            entitySource: EntitySource,
            init: (Builder<ModuleExtensionWorkspaceEntity>.() -> Unit)? = null,
        ): Builder<ModuleExtensionWorkspaceEntity> {
            val builder = builder()
            builder.entitySource = entitySource
            init?.invoke(builder)
            return builder
        }
    }
//endregion
}

//region generated code
var ModuleEntity.Builder.moduleExtensionWorkspaceEntity: @Child ModuleExtensionWorkspaceEntity.Builder<out ModuleExtensionWorkspaceEntity>
        by WorkspaceEntity.extensionBuilder(ModuleExtensionWorkspaceEntity::class.java)
//endregion

val ModuleEntity.moduleExtensionWorkspaceEntity: @Child ModuleExtensionWorkspaceEntity
        by WorkspaceEntity.extension()
