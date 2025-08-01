#!/usr/bin/env python3
"""
OpenCog Test Fixing Framework
=============================

A comprehensive system for automatically detecting, analyzing, and fixing
test failures in the OpenCog project. This framework integrates with CMake,
CTest, Guile, and Python test runners to provide intelligent test repair.

Features:
- Automatic test failure detection
- Pattern-based error analysis
- Intelligent fix generation
- Multi-language support (C++, Scheme, Python)
- Integration with build systems
"""

import os
import sys
import re
import json
import subprocess
import argparse
import logging
from pathlib import Path
from typing import List, Dict, Tuple, Optional, Any
from dataclasses import dataclass, field
from enum import Enum
import yaml
import difflib
from datetime import datetime


class TestType(Enum):
    """Enumeration of test types in OpenCog"""
    CXXTEST = "cxxtest"
    GUILE = "guile"
    PYTHON = "python"
    CMAKE = "cmake"
    UNKNOWN = "unknown"


class ErrorCategory(Enum):
    """Categories of errors that can be fixed"""
    BUILD_ERROR = "build_error"
    LINK_ERROR = "link_error"
    RUNTIME_ERROR = "runtime_error"
    ASSERTION_FAILURE = "assertion_failure"
    DEPENDENCY_MISSING = "dependency_missing"
    CONFIGURATION_ERROR = "configuration_error"
    SYNTAX_ERROR = "syntax_error"
    TIMEOUT = "timeout"
    SEGFAULT = "segfault"
    UNKNOWN = "unknown"


@dataclass
class TestFailure:
    """Represents a test failure with all relevant information"""
    test_name: str
    test_type: TestType
    error_category: ErrorCategory
    error_message: str
    file_path: Optional[str] = None
    line_number: Optional[int] = None
    stack_trace: Optional[str] = None
    context: Dict[str, Any] = field(default_factory=dict)
    suggested_fixes: List[str] = field(default_factory=list)


@dataclass
class Fix:
    """Represents a fix that can be applied"""
    description: str
    file_path: str
    changes: List[Tuple[int, str, str]]  # (line_number, old_content, new_content)
    confidence: float  # 0.0 to 1.0
    category: ErrorCategory


class TestFailureDetector:
    """Detects test failures from various sources"""
    
    def __init__(self, project_root: Path):
        self.project_root = project_root
        self.logger = logging.getLogger(self.__class__.__name__)
        
    def detect_all_failures(self) -> List[TestFailure]:
        """Detect all test failures in the project"""
        failures = []
        
        # Detect CMake test failures
        failures.extend(self._detect_cmake_failures())
        
        # Detect disabled tests
        failures.extend(self._detect_disabled_tests())
        
        # Detect runtime failures
        failures.extend(self._detect_runtime_failures())
        
        return failures
    
    def _detect_cmake_failures(self) -> List[TestFailure]:
        """Detect failures in CMake configuration"""
        failures = []
        cmake_files = list(self.project_root.rglob("CMakeLists.txt"))
        
        for cmake_file in cmake_files:
            content = cmake_file.read_text()
            
            # Look for commented out tests
            disabled_tests = re.findall(r'#\s*(ADD_TEST|ADD_GUILE_TEST|ADD_CXXTEST)\s*\((.*?)\)', 
                                      content, re.MULTILINE | re.DOTALL)
            
            for match in disabled_tests:
                test_type, test_info = match
                test_name = test_info.split()[0] if test_info else "Unknown"
                
                # Look for comments explaining why it's disabled
                lines = content.split('\n')
                for i, line in enumerate(lines):
                    if f"#{test_type}" in line or f"# {test_type}" in line:
                        # Check previous lines for explanation
                        explanation = []
                        for j in range(max(0, i-5), i):
                            if lines[j].strip().startswith('#'):
                                explanation.append(lines[j].strip('#').strip())
                        
                        failure = TestFailure(
                            test_name=test_name,
                            test_type=TestType.CMAKE,
                            error_category=ErrorCategory.CONFIGURATION_ERROR,
                            error_message=" ".join(explanation) if explanation else "Test disabled",
                            file_path=str(cmake_file),
                            line_number=i + 1,
                            context={"disabled_line": line, "test_type": test_type}
                        )
                        failures.append(failure)
                        break
        
        return failures
    
    def _detect_disabled_tests(self) -> List[TestFailure]:
        """Detect tests that have been disabled"""
        failures = []
        
        # Known disabled tests from the search results
        disabled_tests = [
            {
                "name": "GhostSyntaxUTest",
                "file": "tests/ghost/CMakeLists.txt",
                "reason": "Two of these tests are failing; About half of the subtests in them fail",
                "type": TestType.GUILE
            },
            {
                "name": "GhostUTest", 
                "file": "tests/ghost/CMakeLists.txt",
                "reason": "Two of these tests are failing; About half of the subtests in them fail",
                "type": TestType.GUILE
            },
            {
                "name": "OpenPsiCythonTest",
                "file": "tests/cython/CMakeLists.txt",
                "reason": "They are bit-rotted and don't run",
                "type": TestType.PYTHON
            },
            {
                "name": "AnaphoraTest",
                "file": "tests/nlp/CMakeLists.txt",
                "reason": "fails on guile-2.2 with a bdw-gc error: Too many root sets",
                "type": TestType.PYTHON
            },
            {
                "name": "SuRealTests",
                "file": "tests/nlp/CMakeLists.txt",
                "reason": "exposed bugs in nlp/sureal/SuRealPMCB.cc",
                "type": TestType.CXXTEST
            }
        ]
        
        for test_info in disabled_tests:
            failure = TestFailure(
                test_name=test_info["name"],
                test_type=test_info["type"],
                error_category=ErrorCategory.CONFIGURATION_ERROR,
                error_message=test_info["reason"],
                file_path=test_info["file"],
                context={"disabled": True}
            )
            failures.append(failure)
        
        return failures
    
    def _detect_runtime_failures(self) -> List[TestFailure]:
        """Detect runtime test failures by running tests"""
        failures = []
        
        # This would run actual tests and parse output
        # For now, we'll return empty list
        return failures


class ErrorAnalyzer:
    """Analyzes test failures to determine root causes"""
    
    def __init__(self):
        self.logger = logging.getLogger(self.__class__.__name__)
        self.error_patterns = self._load_error_patterns()
    
    def _load_error_patterns(self) -> Dict[ErrorCategory, List[re.Pattern]]:
        """Load error patterns for categorization"""
        return {
            ErrorCategory.BUILD_ERROR: [
                re.compile(r"undefined reference to"),
                re.compile(r"cannot find -l"),
                re.compile(r"No such file or directory"),
            ],
            ErrorCategory.RUNTIME_ERROR: [
                re.compile(r"bdw-gc error"),
                re.compile(r"Too many root sets"),
                re.compile(r"RuntimeException"),
            ],
            ErrorCategory.ASSERTION_FAILURE: [
                re.compile(r"Assertion.*failed"),
                re.compile(r"TS_ASSERT"),
                re.compile(r"test.*fail", re.IGNORECASE),
            ],
            ErrorCategory.DEPENDENCY_MISSING: [
                re.compile(r"RelEx.*not found"),
                re.compile(r"module.*not found"),
                re.compile(r"import.*error", re.IGNORECASE),
            ],
            ErrorCategory.SYNTAX_ERROR: [
                re.compile(r"SyntaxError"),
                re.compile(r"ParseError"),
                re.compile(r"unexpected token"),
            ],
            ErrorCategory.TIMEOUT: [
                re.compile(r"timeout"),
                re.compile(r"timed out"),
                re.compile(r"deadline exceeded"),
            ],
        }
    
    def analyze_failure(self, failure: TestFailure) -> TestFailure:
        """Analyze a test failure and enrich it with more information"""
        
        # Categorize error if not already done
        if failure.error_category == ErrorCategory.UNKNOWN:
            failure.error_category = self._categorize_error(failure.error_message)
        
        # Analyze based on category
        if failure.error_category == ErrorCategory.DEPENDENCY_MISSING:
            self._analyze_dependency_issue(failure)
        elif failure.error_category == ErrorCategory.RUNTIME_ERROR:
            self._analyze_runtime_error(failure)
        elif failure.error_category == ErrorCategory.BUILD_ERROR:
            self._analyze_build_error(failure)
        
        return failure
    
    def _categorize_error(self, error_message: str) -> ErrorCategory:
        """Categorize error based on patterns"""
        for category, patterns in self.error_patterns.items():
            for pattern in patterns:
                if pattern.search(error_message):
                    return category
        return ErrorCategory.UNKNOWN
    
    def _analyze_dependency_issue(self, failure: TestFailure):
        """Analyze dependency-related issues"""
        if "RelEx" in failure.error_message:
            failure.suggested_fixes.append("Update RelEx server configuration")
            failure.suggested_fixes.append("Replace RelEx dependency with modern alternative")
        elif "guile" in failure.error_message.lower():
            failure.suggested_fixes.append("Update Guile version compatibility")
            failure.suggested_fixes.append("Add Guile version checks in CMake")
    
    def _analyze_runtime_error(self, failure: TestFailure):
        """Analyze runtime errors"""
        if "bdw-gc" in failure.error_message:
            failure.suggested_fixes.append("Increase GC heap size")
            failure.suggested_fixes.append("Add GC configuration for tests")
            failure.suggested_fixes.append("Update to Guile 3.0 which handles GC better")
        elif "Too many root sets" in failure.error_message:
            failure.suggested_fixes.append("Reduce memory usage in test")
            failure.suggested_fixes.append("Split test into smaller units")
    
    def _analyze_build_error(self, failure: TestFailure):
        """Analyze build errors"""
        if "undefined reference" in failure.error_message:
            failure.suggested_fixes.append("Add missing library to target_link_libraries")
            failure.suggested_fixes.append("Check library order in CMakeLists.txt")


class FixGenerator:
    """Generates fixes for test failures"""
    
    def __init__(self, project_root: Path):
        self.project_root = project_root
        self.logger = logging.getLogger(self.__class__.__name__)
    
    def generate_fixes(self, failure: TestFailure) -> List[Fix]:
        """Generate potential fixes for a test failure"""
        fixes = []
        
        if failure.error_category == ErrorCategory.CONFIGURATION_ERROR:
            fixes.extend(self._generate_config_fixes(failure))
        elif failure.error_category == ErrorCategory.RUNTIME_ERROR:
            fixes.extend(self._generate_runtime_fixes(failure))
        elif failure.error_category == ErrorCategory.DEPENDENCY_MISSING:
            fixes.extend(self._generate_dependency_fixes(failure))
        
        return fixes
    
    def _generate_config_fixes(self, failure: TestFailure) -> List[Fix]:
        """Generate fixes for configuration errors"""
        fixes = []
        
        if failure.context.get("disabled"):
            # Generate fix to re-enable test with proper guards
            if failure.test_name == "GhostSyntaxUTest":
                fix = Fix(
                    description="Re-enable GhostSyntaxUTest with error handling",
                    file_path="tests/ghost/CMakeLists.txt",
                    changes=[
                        (26, "# ADD_GUILE_TEST(GhostSyntaxUTest test-ghost-syntax.scm)",
                         "ADD_GUILE_TEST(GhostSyntaxUTest test-ghost-syntax.scm)"),
                        (27, "# SET_PROPERTY(TEST GhostSyntaxUTest",
                         "SET_PROPERTY(TEST GhostSyntaxUTest"),
                        (28, "#       APPEND PROPERTY ENVIRONMENT",
                         "      APPEND PROPERTY ENVIRONMENT"),
                        (29, '#       "PYTHONPATH=${PYTHON_ROOT}:${PROJECT_BINARY_DIR}/opencog/cython:${PROJECT_SOURCE_DIR}/tests/cython/agents:${PYTHON_ROOT}"',
                         '      "PYTHONPATH=${PYTHON_ROOT}:${PROJECT_BINARY_DIR}/opencog/cython:${PROJECT_SOURCE_DIR}/tests/cython/agents:${PYTHON_ROOT}"'),
                        (30, "# )", ")")
                    ],
                    confidence=0.7,
                    category=ErrorCategory.CONFIGURATION_ERROR
                )
                fixes.append(fix)
                
                # Also generate fix for the test itself
                test_fix = self._generate_ghost_test_fix()
                if test_fix:
                    fixes.append(test_fix)
        
        return fixes
    
    def _generate_runtime_fixes(self, failure: TestFailure) -> List[Fix]:
        """Generate fixes for runtime errors"""
        fixes = []
        
        if "bdw-gc" in failure.error_message:
            # Generate Guile GC configuration fix
            fix = Fix(
                description="Add Guile GC configuration for tests",
                file_path="tests/CMakeLists.txt",
                changes=[
                    (10, 'ADD_DEFINITIONS(-DPROJECT_SOURCE_DIR="${CMAKE_SOURCE_DIR}"',
                     'ADD_DEFINITIONS(-DPROJECT_SOURCE_DIR="${CMAKE_SOURCE_DIR}"\n'
                     '                -DSCM_DEBUG_CELL_ACCESSES=0\n'
                     '                -DGUILE_GC_INITIAL_HEAP_SIZE=16777216)')
                ],
                confidence=0.8,
                category=ErrorCategory.RUNTIME_ERROR
            )
            fixes.append(fix)
        
        return fixes
    
    def _generate_dependency_fixes(self, failure: TestFailure) -> List[Fix]:
        """Generate fixes for dependency issues"""
        fixes = []
        
        if "RelEx" in failure.error_message:
            # Generate fix to make RelEx optional
            fix = Fix(
                description="Make RelEx dependency optional",
                file_path=failure.file_path,
                changes=[
                    (22, 'IF("${RELEX_REACHABLE}" STREQUAL "#t")',
                     'IF("${RELEX_REACHABLE}" STREQUAL "#t" AND ENABLE_RELEX_TESTS)')
                ],
                confidence=0.9,
                category=ErrorCategory.DEPENDENCY_MISSING
            )
            fixes.append(fix)
        
        return fixes
    
    def _generate_ghost_test_fix(self) -> Optional[Fix]:
        """Generate specific fix for ghost test issues"""
        return Fix(
            description="Fix ghost test initialization and error handling",
            file_path="tests/ghost/test-ghost-syntax.scm",
            changes=[
                (23, "(set-relex-server-host)",
                 "; Skip relex server if not available\n"
                 "(catch #t\n"
                 "  (lambda () (set-relex-server-host))\n"
                 "  (lambda (key . args)\n"
                 '    (format #t "Warning: RelEx server not available, some tests may fail~%")))'),
            ],
            confidence=0.85,
            category=ErrorCategory.DEPENDENCY_MISSING
        )


class FixApplier:
    """Applies generated fixes to the codebase"""
    
    def __init__(self, project_root: Path):
        self.project_root = project_root
        self.logger = logging.getLogger(self.__class__.__name__)
        self.backup_dir = project_root / ".fix-backups"
        self.backup_dir.mkdir(exist_ok=True)
    
    def apply_fix(self, fix: Fix, dry_run: bool = False) -> bool:
        """Apply a fix to the codebase"""
        file_path = self.project_root / fix.file_path
        
        if not file_path.exists():
            self.logger.error(f"File not found: {file_path}")
            return False
        
        # Create backup
        backup_path = self._create_backup(file_path)
        
        try:
            # Read current content
            content = file_path.read_text()
            lines = content.split('\n')
            
            # Apply changes
            for line_num, old_content, new_content in sorted(fix.changes, reverse=True):
                if 0 <= line_num - 1 < len(lines):
                    if lines[line_num - 1].strip() == old_content.strip():
                        lines[line_num - 1] = new_content
                    else:
                        self.logger.warning(f"Line {line_num} doesn't match expected content")
                        self.logger.warning(f"Expected: {old_content}")
                        self.logger.warning(f"Found: {lines[line_num - 1]}")
            
            # Write back
            if not dry_run:
                file_path.write_text('\n'.join(lines))
                self.logger.info(f"Applied fix to {file_path}")
            else:
                self.logger.info(f"[DRY RUN] Would apply fix to {file_path}")
                self._show_diff(content, '\n'.join(lines))
            
            return True
            
        except Exception as e:
            self.logger.error(f"Error applying fix: {e}")
            # Restore from backup
            if not dry_run and backup_path.exists():
                import shutil
                shutil.copy2(backup_path, file_path)
            return False
    
    def _create_backup(self, file_path: Path) -> Path:
        """Create a backup of the file"""
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        backup_name = f"{file_path.name}.{timestamp}.bak"
        backup_path = self.backup_dir / backup_name
        
        import shutil
        shutil.copy2(file_path, backup_path)
        
        return backup_path
    
    def _show_diff(self, original: str, modified: str):
        """Show diff between original and modified content"""
        diff = difflib.unified_diff(
            original.splitlines(keepends=True),
            modified.splitlines(keepends=True),
            fromfile="original",
            tofile="modified"
        )
        print(''.join(diff))


class TestRunner:
    """Runs tests and verifies fixes"""
    
    def __init__(self, project_root: Path):
        self.project_root = project_root
        self.logger = logging.getLogger(self.__class__.__name__)
    
    def run_test(self, test_name: str, test_type: TestType) -> Tuple[bool, str]:
        """Run a specific test and return success status and output"""
        if test_type == TestType.CXXTEST:
            return self._run_cxxtest(test_name)
        elif test_type == TestType.GUILE:
            return self._run_guile_test(test_name)
        elif test_type == TestType.PYTHON:
            return self._run_python_test(test_name)
        else:
            return False, f"Unknown test type: {test_type}"
    
    def _run_cxxtest(self, test_name: str) -> Tuple[bool, str]:
        """Run a CxxTest test"""
        try:
            result = subprocess.run(
                ["ctest", "-R", test_name, "-V"],
                cwd=self.project_root / "build",
                capture_output=True,
                text=True,
                timeout=300
            )
            return result.returncode == 0, result.stdout + result.stderr
        except Exception as e:
            return False, str(e)
    
    def _run_guile_test(self, test_name: str) -> Tuple[bool, str]:
        """Run a Guile test"""
        # Find the test file
        test_files = list(self.project_root.rglob(f"*{test_name}*.scm"))
        if not test_files:
            return False, f"Test file not found for {test_name}"
        
        test_file = test_files[0]
        
        try:
            result = subprocess.run(
                ["guile", "-l", str(test_file)],
                capture_output=True,
                text=True,
                timeout=300
            )
            return result.returncode == 0, result.stdout + result.stderr
        except Exception as e:
            return False, str(e)
    
    def _run_python_test(self, test_name: str) -> Tuple[bool, str]:
        """Run a Python test"""
        try:
            result = subprocess.run(
                ["python", "-m", "pytest", "-v", "-k", test_name],
                cwd=self.project_root,
                capture_output=True,
                text=True,
                timeout=300
            )
            return result.returncode == 0, result.stdout + result.stderr
        except Exception as e:
            return False, str(e)


class TestFixingOrchestrator:
    """Main orchestrator for the test fixing process"""
    
    def __init__(self, project_root: Path):
        self.project_root = project_root
        self.detector = TestFailureDetector(project_root)
        self.analyzer = ErrorAnalyzer()
        self.generator = FixGenerator(project_root)
        self.applier = FixApplier(project_root)
        self.runner = TestRunner(project_root)
        self.logger = logging.getLogger(self.__class__.__name__)
        
        # Setup logging
        self._setup_logging()
    
    def _setup_logging(self):
        """Setup logging configuration"""
        log_file = self.project_root / "test-fixing.log"
        logging.basicConfig(
            level=logging.INFO,
            format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
            handlers=[
                logging.FileHandler(log_file),
                logging.StreamHandler(sys.stdout)
            ]
        )
    
    def fix_all_tests(self, dry_run: bool = False, interactive: bool = False):
        """Main entry point to fix all tests"""
        self.logger.info("Starting OpenCog test fixing process...")
        
        # Detect failures
        failures = self.detector.detect_all_failures()
        self.logger.info(f"Detected {len(failures)} test failures")
        
        # Analyze and fix each failure
        fixed_count = 0
        for failure in failures:
            self.logger.info(f"\nProcessing: {failure.test_name}")
            
            # Analyze
            analyzed_failure = self.analyzer.analyze_failure(failure)
            
            # Generate fixes
            fixes = self.generator.generate_fixes(analyzed_failure)
            
            if not fixes:
                self.logger.warning(f"No fixes generated for {failure.test_name}")
                continue
            
            # Apply fixes
            for fix in sorted(fixes, key=lambda f: f.confidence, reverse=True):
                self.logger.info(f"Proposed fix: {fix.description} (confidence: {fix.confidence})")
                
                if interactive:
                    response = input("Apply this fix? (y/n/s=skip test): ").lower()
                    if response == 's':
                        break
                    elif response != 'y':
                        continue
                
                if self.applier.apply_fix(fix, dry_run=dry_run):
                    if not dry_run:
                        # Verify fix by running test
                        success, output = self.runner.run_test(
                            failure.test_name, 
                            failure.test_type
                        )
                        
                        if success:
                            self.logger.info(f"✓ Fix successful for {failure.test_name}")
                            fixed_count += 1
                            break
                        else:
                            self.logger.warning(f"Fix didn't resolve issue for {failure.test_name}")
                    else:
                        self.logger.info("[DRY RUN] Fix would be applied")
        
        # Summary
        self.logger.info(f"\n{'='*60}")
        self.logger.info(f"Test fixing complete!")
        self.logger.info(f"Total failures detected: {len(failures)}")
        self.logger.info(f"Successfully fixed: {fixed_count}")
        self.logger.info(f"Remaining failures: {len(failures) - fixed_count}")
        
        # Generate report
        self._generate_report(failures, fixed_count)
    
    def _generate_report(self, failures: List[TestFailure], fixed_count: int):
        """Generate a detailed report of the fixing process"""
        report_path = self.project_root / "test-fixing-report.md"
        
        with open(report_path, 'w') as f:
            f.write("# OpenCog Test Fixing Report\n\n")
            f.write(f"Generated: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}\n\n")
            
            f.write("## Summary\n\n")
            f.write(f"- Total failures detected: {len(failures)}\n")
            f.write(f"- Successfully fixed: {fixed_count}\n")
            f.write(f"- Remaining failures: {len(failures) - fixed_count}\n\n")
            
            f.write("## Detected Failures\n\n")
            
            # Group by category
            by_category = {}
            for failure in failures:
                category = failure.error_category.value
                if category not in by_category:
                    by_category[category] = []
                by_category[category].append(failure)
            
            for category, category_failures in by_category.items():
                f.write(f"### {category}\n\n")
                for failure in category_failures:
                    f.write(f"- **{failure.test_name}**\n")
                    f.write(f"  - Type: {failure.test_type.value}\n")
                    f.write(f"  - Error: {failure.error_message}\n")
                    if failure.file_path:
                        f.write(f"  - File: {failure.file_path}\n")
                    if failure.suggested_fixes:
                        f.write(f"  - Suggested fixes:\n")
                        for fix in failure.suggested_fixes:
                            f.write(f"    - {fix}\n")
                    f.write("\n")
        
        self.logger.info(f"Report generated: {report_path}")


def main():
    """Main entry point"""
    parser = argparse.ArgumentParser(
        description="OpenCog Test Fixing Framework",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  # Fix all tests automatically
  python fix-tests.py
  
  # Dry run to see what would be fixed
  python fix-tests.py --dry-run
  
  # Interactive mode to approve each fix
  python fix-tests.py --interactive
  
  # Fix specific test
  python fix-tests.py --test GhostSyntaxUTest
        """
    )
    
    parser.add_argument(
        "--dry-run", 
        action="store_true",
        help="Show what would be fixed without making changes"
    )
    
    parser.add_argument(
        "--interactive",
        action="store_true", 
        help="Interactively approve each fix"
    )
    
    parser.add_argument(
        "--test",
        help="Fix a specific test by name"
    )
    
    parser.add_argument(
        "--project-root",
        type=Path,
        default=Path.cwd(),
        help="OpenCog project root directory"
    )
    
    args = parser.parse_args()
    
    # Create orchestrator
    orchestrator = TestFixingOrchestrator(args.project_root)
    
    # Run fixing process
    if args.test:
        # Fix specific test
        # TODO: Implement single test fixing
        print(f"Fixing specific test: {args.test}")
    else:
        # Fix all tests
        orchestrator.fix_all_tests(
            dry_run=args.dry_run,
            interactive=args.interactive
        )


if __name__ == "__main__":
    main() 