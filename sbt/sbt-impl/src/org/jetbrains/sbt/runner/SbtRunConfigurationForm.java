package org.jetbrains.sbt.runner;

import com.intellij.execution.configuration.EnvironmentVariablesComponent;
import com.intellij.openapi.fileChooser.FileChooserDescriptor;
import com.intellij.openapi.fileChooser.FileChooserDescriptorFactory;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.TextFieldWithBrowseButton;
import com.intellij.ui.RawCommandLineEditor;
import com.intellij.uiDesigner.core.GridConstraints;
import com.intellij.uiDesigner.core.GridLayoutManager;
import com.intellij.uiDesigner.core.Spacer;
import org.jetbrains.sbt.SbtBundle;

import javax.swing.*;
import java.awt.*;
import java.lang.reflect.Method;
import java.util.List;
import java.util.Map;
import java.util.ResourceBundle;

/**
 * Form with configuration of sbt runner.
 */
@SuppressWarnings("deprecation")
public class SbtRunConfigurationForm {
    private JPanel mainPanel;
    private RawCommandLineEditor tasksEditor;
    private RawCommandLineEditor javaOptionsEditor;
    private EnvironmentVariablesComponent environmentVariables;
    private TextFieldWithBrowseButton workingDirField;
    private JCheckBox useSbtShellCheckBox;
    private JPanel optionalPanel;

    public SbtRunConfigurationForm(final Project project, final SbtRunConfiguration configuration) {
        environmentVariables.setEnvs(configuration.environmentVariables());
        environmentVariables.setEnvFilePaths(configuration.envFilePaths());
        workingDirField.setText(configuration.getWorkingDir());
        FileChooserDescriptor folderDescriptor = FileChooserDescriptorFactory.createSingleFolderDescriptor();
        workingDirField.addBrowseFolderListener(SbtBundle.message("sbt.runner.choose.working.directory"), null, project, folderDescriptor);

        setCustomOptionsEnabled(!useSbtShellCheckBox.isSelected());
        useSbtShellCheckBox.addChangeListener(e -> setCustomOptionsEnabled(!useSbtShellCheckBox.isSelected()));
    }

    public JPanel getMainPanel() {
        return mainPanel;
    }

    /**
     * @return tasks to execute.
     */
    public String getTasks() {
        return tasksEditor.getText();
    }

    /**
     * @return java options.
     */
    public String getJavaOptions() {
        return javaOptionsEditor.getText();
    }

    /**
     * @return envirnoment variables.
     */
    public Map<String, String> getEnvironmentVariables() {
        return environmentVariables.getEnvs();
    }

    public List<String> getEnvFilePaths() {
        return environmentVariables.getEnvFilePaths();
    }

    public String getWorkingDir() {
        return workingDirField.getText();
    }

    public boolean isUseSbtShell() {
        return useSbtShellCheckBox.isSelected();
    }

    public void apply(SbtRunConfiguration configuration) {
        tasksEditor.setText(configuration.getTasks());
        javaOptionsEditor.setText(configuration.getVmparams());
        environmentVariables.setEnvs(configuration.environmentVariables());
        environmentVariables.setEnvFilePaths(configuration.envFilePaths());
        workingDirField.setText(configuration.getWorkingDir());
        useSbtShellCheckBox.setSelected(configuration.getUseSbtShell());
    }

    private void setCustomOptionsEnabled(boolean enabled) {
        for (Component c : optionalPanel.getComponents()) c.setEnabled(enabled);
    }

    {
// GUI initializer generated by IntelliJ IDEA GUI Designer
// >>> IMPORTANT!! <<<
// DO NOT EDIT OR ADD ANY CODE HERE!
        $$$setupUI$$$();
    }

    /**
     * Method generated by IntelliJ IDEA GUI Designer
     * >>> IMPORTANT!! <<<
     * DO NOT edit this method OR call it in your code!
     *
     * @noinspection ALL
     */
    private void $$$setupUI$$$() {
        mainPanel = new JPanel();
        mainPanel.setLayout(new GridLayoutManager(9, 2, new Insets(0, 0, 0, 0), -1, -1));
        final JLabel label1 = new JLabel();
        this.$$$loadLabelText$$$(label1, this.$$$getMessageFromBundle$$$("messages/SbtBundle", "sbt.runner.form.tasks"));
        mainPanel.add(label1, new GridConstraints(0, 0, 1, 2, GridConstraints.ANCHOR_WEST, GridConstraints.FILL_NONE, GridConstraints.SIZEPOLICY_FIXED, GridConstraints.SIZEPOLICY_FIXED, null, null, null, 0, false));
        tasksEditor = new RawCommandLineEditor();
        tasksEditor.setDialogCaption(this.$$$getMessageFromBundle$$$("messages/SbtBundle", "sbt.runner.form.tasksEditorTitle"));
        mainPanel.add(tasksEditor, new GridConstraints(1, 0, 1, 2, GridConstraints.ANCHOR_NORTH, GridConstraints.FILL_HORIZONTAL, GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_CAN_GROW, GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_CAN_GROW, null, new Dimension(495, 29), null, 0, false));
        useSbtShellCheckBox = new JCheckBox();
        this.$$$loadButtonText$$$(useSbtShellCheckBox, this.$$$getMessageFromBundle$$$("messages/SbtBundle", "sbt.runner.form.use.sbt.shell"));
        mainPanel.add(useSbtShellCheckBox, new GridConstraints(2, 0, 1, 2, GridConstraints.ANCHOR_WEST, GridConstraints.FILL_NONE, GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_CAN_GROW, GridConstraints.SIZEPOLICY_FIXED, null, null, null, 0, false));
        optionalPanel = new JPanel();
        optionalPanel.setLayout(new GridLayoutManager(6, 2, new Insets(0, 0, 0, 0), -1, -1));
        mainPanel.add(optionalPanel, new GridConstraints(3, 0, 6, 2, GridConstraints.ANCHOR_CENTER, GridConstraints.FILL_BOTH, GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_CAN_GROW, GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_CAN_GROW, null, null, null, 0, false));
        javaOptionsEditor = new RawCommandLineEditor();
        javaOptionsEditor.setDialogCaption(this.$$$getMessageFromBundle$$$("messages/SbtBundle", "sbt.runner.form.vmParametersEditorTitle"));
        optionalPanel.add(javaOptionsEditor, new GridConstraints(3, 1, 1, 1, GridConstraints.ANCHOR_CENTER, GridConstraints.FILL_HORIZONTAL, GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_CAN_GROW, GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_CAN_GROW, null, new Dimension(495, 29), null, 0, false));
        final JLabel label2 = new JLabel();
        this.$$$loadLabelText$$$(label2, this.$$$getMessageFromBundle$$$("messages/SbtBundle", "sbt.runner.form.vmParameters"));
        optionalPanel.add(label2, new GridConstraints(2, 1, 1, 1, GridConstraints.ANCHOR_WEST, GridConstraints.FILL_NONE, GridConstraints.SIZEPOLICY_FIXED, GridConstraints.SIZEPOLICY_FIXED, null, new Dimension(495, 21), null, 0, false));
        environmentVariables = new EnvironmentVariablesComponent();
        environmentVariables.setText(this.$$$getMessageFromBundle$$$("messages/SbtBundle", "sbt.runner.form.environmentVariables"));
        optionalPanel.add(environmentVariables, new GridConstraints(4, 1, 1, 1, GridConstraints.ANCHOR_CENTER, GridConstraints.FILL_HORIZONTAL, GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_CAN_GROW, GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_CAN_GROW, null, null, null, 0, false));
        final Spacer spacer1 = new Spacer();
        optionalPanel.add(spacer1, new GridConstraints(5, 1, 1, 1, GridConstraints.ANCHOR_CENTER, GridConstraints.FILL_VERTICAL, 1, GridConstraints.SIZEPOLICY_WANT_GROW, null, null, null, 0, false));
        workingDirField = new TextFieldWithBrowseButton();
        optionalPanel.add(workingDirField, new GridConstraints(1, 1, 1, 1, GridConstraints.ANCHOR_CENTER, GridConstraints.FILL_HORIZONTAL, GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_CAN_GROW, GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_CAN_GROW, null, null, null, 0, false));
        final JLabel label3 = new JLabel();
        this.$$$loadLabelText$$$(label3, this.$$$getMessageFromBundle$$$("messages/SbtBundle", "sbt.runner.form.working.directory"));
        optionalPanel.add(label3, new GridConstraints(0, 1, 1, 1, GridConstraints.ANCHOR_WEST, GridConstraints.FILL_NONE, GridConstraints.SIZEPOLICY_FIXED, GridConstraints.SIZEPOLICY_FIXED, null, null, null, 0, false));
        final JSeparator separator1 = new JSeparator();
        separator1.setBackground(new Color(-986896));
        separator1.setForeground(new Color(-2105377));
        separator1.setOrientation(1);
        optionalPanel.add(separator1, new GridConstraints(0, 0, 6, 1, GridConstraints.ANCHOR_CENTER, GridConstraints.FILL_VERTICAL, GridConstraints.SIZEPOLICY_FIXED, GridConstraints.SIZEPOLICY_WANT_GROW, null, null, null, 0, false));
    }

    private static Method $$$cachedGetBundleMethod$$$ = null;

    private String $$$getMessageFromBundle$$$(String path, String key) {
        ResourceBundle bundle;
        try {
            Class<?> thisClass = this.getClass();
            if ($$$cachedGetBundleMethod$$$ == null) {
                Class<?> dynamicBundleClass = thisClass.getClassLoader().loadClass("com.intellij.DynamicBundle");
                $$$cachedGetBundleMethod$$$ = dynamicBundleClass.getMethod("getBundle", String.class, Class.class);
            }
            bundle = (ResourceBundle) $$$cachedGetBundleMethod$$$.invoke(null, path, thisClass);
        } catch (Exception e) {
            bundle = ResourceBundle.getBundle(path);
        }
        return bundle.getString(key);
    }

    /**
     * @noinspection ALL
     */
    private void $$$loadLabelText$$$(JLabel component, String text) {
        StringBuffer result = new StringBuffer();
        boolean haveMnemonic = false;
        char mnemonic = '\0';
        int mnemonicIndex = -1;
        for (int i = 0; i < text.length(); i++) {
            if (text.charAt(i) == '&') {
                i++;
                if (i == text.length()) break;
                if (!haveMnemonic && text.charAt(i) != '&') {
                    haveMnemonic = true;
                    mnemonic = text.charAt(i);
                    mnemonicIndex = result.length();
                }
            }
            result.append(text.charAt(i));
        }
        component.setText(result.toString());
        if (haveMnemonic) {
            component.setDisplayedMnemonic(mnemonic);
            component.setDisplayedMnemonicIndex(mnemonicIndex);
        }
    }

    /**
     * @noinspection ALL
     */
    private void $$$loadButtonText$$$(AbstractButton component, String text) {
        StringBuffer result = new StringBuffer();
        boolean haveMnemonic = false;
        char mnemonic = '\0';
        int mnemonicIndex = -1;
        for (int i = 0; i < text.length(); i++) {
            if (text.charAt(i) == '&') {
                i++;
                if (i == text.length()) break;
                if (!haveMnemonic && text.charAt(i) != '&') {
                    haveMnemonic = true;
                    mnemonic = text.charAt(i);
                    mnemonicIndex = result.length();
                }
            }
            result.append(text.charAt(i));
        }
        component.setText(result.toString());
        if (haveMnemonic) {
            component.setMnemonic(mnemonic);
            component.setDisplayedMnemonicIndex(mnemonicIndex);
        }
    }

    /**
     * @noinspection ALL
     */
    public JComponent $$$getRootComponent$$$() {
        return mainPanel;
    }

}
