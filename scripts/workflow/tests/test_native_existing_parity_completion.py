from pathlib import Path
from test_native_reference_parity_completion import NativeReferenceParityCompletionTests


class NativeExistingParityCompletionTests(NativeReferenceParityCompletionTests):
    @classmethod
    def setUpClass(cls):
        super().setUpClass()
        cls.wrapper = cls.root / "scripts/workflow/run_native_existing_parity_completion.sh"
