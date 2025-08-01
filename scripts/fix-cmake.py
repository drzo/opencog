#!/usr/bin/env python3
"""
OpenCog CMake Build System Fixer
================================

Specialized tool for fixing CMake configuration issues in the OpenCog project.
This tool can detect and fix common CMake problems including:
- Missing dependencies
- Incorrect library linking
- Version compatibility issues
- Platform-specific configuration problems
"""

import os
import re
import sys
import json
import subprocess
import argparse
from pathlib import Path
from typing import List, Dict, Tuple, Optional, Set
from dataclasses import dataclass
from enum import Enum
import logging


class CMakeIssueType(Enum):
    """Types of CMake issues that can be fixed"""
    MISSING_FIND_PACKAGE = "missing_find_package"
    INCORRECT_LINK_LIBRARIES = "incorrect_link_libraries"
    VERSION_MISMATCH = "version_mismatch"
    MISSING_INCLUDE_DIR = "missing_include_dir"
    DEPRECATED_COMMAND = "deprecated_command"
    PLATFORM_SPECIFIC = "platform_specific"
    TEST_CONFIGURATION = "test_configuration"
    INSTALL_CONFIGURATION = "install_configuration"


@dataclass
class CMakeIssue:
    """Represents a CMake configuration issue"""
    file_path: Path
    line_number: int
    issue_type: CMakeIssueType
    description: str
    severity: str  # "error", "warning", "info"
    suggested_fix: Optional[str] = None
    context: Dict = None


class CMakeAnalyzer:
    """Analyzes CMake files for issues"""
    
    def __init__(self, project_root: Path):
        self.project_root = project_root
        self.logger = logging.getLogger(self.__class__.__name__)
        self.cmake_files = list(project_root.rglob("CMakeLists.txt")) + \
                          list(project_root.rglob("*.cmake"))
    
    def analyze_all(self) -> List[CMakeIssue]:
        """Analyze all CMake files in the project"""
        issues = []
        
        for cmake_file in self.cmake_files:
            self.logger.info(f"Analyzing {cmake_file}")
            file_issues = self.analyze_file(cmake_file)
            issues.extend(file_issues)
        
        # Also check for cross-file issues
        issues.extend(self._check_dependency_consistency())
        issues.extend(self._check_version_requirements())
        
        return issues
    
    def analyze_file(self, file_path: Path) -> List[CMakeIssue]:
        """Analyze a single CMake file"""
        issues = []
        
        try:
            content = file_path.read_text()
            lines = content.split('\n')
            
            for i, line in enumerate(lines, 1):
                # Check for various issues
                issues.extend(self._check_find_package(file_path, i, line))
                issues.extend(self._check_link_libraries(file_path, i, line))
                issues.extend(self._check_deprecated_commands(file_path, i, line))
                issues.extend(self._check_test_configuration(file_path, i, line))
                
            # Check file-level issues
            issues.extend(self._check_cmake_minimum_version(file_path, content))
            issues.extend(self._check_project_structure(file_path, content))
            
        except Exception as e:
            self.logger.error(f"Error analyzing {file_path}: {e}")
        
        return issues
    
    def _check_find_package(self, file_path: Path, line_num: int, line: str) -> List[CMakeIssue]:
        """Check for find_package issues"""
        issues = []
        
        # Check for find_package without version
        match = re.match(r'\s*find_package\s*\(\s*(\w+)\s*\)', line, re.IGNORECASE)
        if match:
            package = match.group(1)
            if package not in ["Threads", "PkgConfig"]:  # Some packages don't need versions
                issues.append(CMakeIssue(
                    file_path=file_path,
                    line_number=line_num,
                    issue_type=CMakeIssueType.VERSION_MISMATCH,
                    description=f"find_package({package}) without version specification",
                    severity="warning",
                    suggested_fix=f"find_package({package} REQUIRED) or specify version"
                ))
        
        # Check for missing REQUIRED
        if "find_package" in line.lower() and "REQUIRED" not in line:
            issues.append(CMakeIssue(
                file_path=file_path,
                line_number=line_num,
                issue_type=CMakeIssueType.MISSING_FIND_PACKAGE,
                description="find_package without REQUIRED keyword",
                severity="info",
                suggested_fix="Add REQUIRED keyword if this is a mandatory dependency"
            ))
        
        return issues
    
    def _check_link_libraries(self, file_path: Path, line_num: int, line: str) -> List[CMakeIssue]:
        """Check for target_link_libraries issues"""
        issues = []
        
        if "target_link_libraries" in line.lower():
            # Check for old-style linking without visibility keywords
            if not any(keyword in line for keyword in ["PUBLIC", "PRIVATE", "INTERFACE"]):
                issues.append(CMakeIssue(
                    file_path=file_path,
                    line_number=line_num,
                    issue_type=CMakeIssueType.INCORRECT_LINK_LIBRARIES,
                    description="target_link_libraries without visibility keywords",
                    severity="warning",
                    suggested_fix="Add PUBLIC, PRIVATE, or INTERFACE keyword"
                ))
        
        return issues
    
    def _check_deprecated_commands(self, file_path: Path, line_num: int, line: str) -> List[CMakeIssue]:
        """Check for deprecated CMake commands"""
        issues = []
        
        deprecated_commands = {
            "include_directories": "Use target_include_directories instead",
            "link_directories": "Use target_link_directories or find_package instead",
            "add_definitions": "Use target_compile_definitions instead",
            "set(CMAKE_CXX_FLAGS": "Use target_compile_options instead"
        }
        
        for cmd, suggestion in deprecated_commands.items():
            if cmd in line and not line.strip().startswith("#"):
                issues.append(CMakeIssue(
                    file_path=file_path,
                    line_number=line_num,
                    issue_type=CMakeIssueType.DEPRECATED_COMMAND,
                    description=f"Deprecated command: {cmd}",
                    severity="warning",
                    suggested_fix=suggestion
                ))
        
        return issues
    
    def _check_test_configuration(self, file_path: Path, line_num: int, line: str) -> List[CMakeIssue]:
        """Check for test configuration issues"""
        issues = []
        
        # Check for tests without proper properties
        if re.match(r'\s*add_test\s*\(', line, re.IGNORECASE):
            # Look for common test issues
            if "WORKING_DIRECTORY" not in line:
                # Check next few lines for set_tests_properties
                # This is a simplified check
                issues.append(CMakeIssue(
                    file_path=file_path,
                    line_number=line_num,
                    issue_type=CMakeIssueType.TEST_CONFIGURATION,
                    description="Test may need WORKING_DIRECTORY property",
                    severity="info",
                    suggested_fix="Consider setting WORKING_DIRECTORY property"
                ))
        
        return issues
    
    def _check_cmake_minimum_version(self, file_path: Path, content: str) -> List[CMakeIssue]:
        """Check CMAKE_MINIMUM_REQUIRED version"""
        issues = []
        
        match = re.search(r'cmake_minimum_required\s*\(\s*VERSION\s+(\d+\.\d+)', content, re.IGNORECASE)
        if match:
            version = float(match.group(1))
            if version < 3.0:
                issues.append(CMakeIssue(
                    file_path=file_path,
                    line_number=1,
                    issue_type=CMakeIssueType.VERSION_MISMATCH,
                    description=f"CMake minimum version {version} is too old",
                    severity="warning",
                    suggested_fix="Update to CMAKE_MINIMUM_REQUIRED(VERSION 3.0) or higher"
                ))
        
        return issues
    
    def _check_project_structure(self, file_path: Path, content: str) -> List[CMakeIssue]:
        """Check for proper project structure"""
        issues = []
        
        # Check if it's a top-level CMakeLists.txt
        if file_path.name == "CMakeLists.txt" and file_path.parent == self.project_root:
            if "project(" not in content.lower():
                issues.append(CMakeIssue(
                    file_path=file_path,
                    line_number=1,
                    issue_type=CMakeIssueType.MISSING_FIND_PACKAGE,
                    description="Missing PROJECT command in top-level CMakeLists.txt",
                    severity="error",
                    suggested_fix="Add PROJECT(opencog) command"
                ))
        
        return issues
    
    def _check_dependency_consistency(self) -> List[CMakeIssue]:
        """Check for consistency in dependency usage across files"""
        issues = []
        
        # Track which packages are found where
        package_usage = {}
        
        for cmake_file in self.cmake_files:
            content = cmake_file.read_text()
            packages = re.findall(r'find_package\s*\(\s*(\w+)', content, re.IGNORECASE)
            
            for package in packages:
                if package not in package_usage:
                    package_usage[package] = []
                package_usage[package].append(cmake_file)
        
        # Check for packages that should be found at top level
        top_level_packages = ["Boost", "Guile", "Python", "CxxTest"]
        top_cmake = self.project_root / "CMakeLists.txt"
        
        for package in top_level_packages:
            if package in package_usage and top_cmake not in package_usage[package]:
                for file_path in package_usage[package]:
                    issues.append(CMakeIssue(
                        file_path=file_path,
                        line_number=1,
                        issue_type=CMakeIssueType.MISSING_FIND_PACKAGE,
                        description=f"{package} should be found in top-level CMakeLists.txt",
                        severity="warning",
                        suggested_fix=f"Move find_package({package}) to top-level CMakeLists.txt"
                    ))
        
        return issues
    
    def _check_version_requirements(self) -> List[CMakeIssue]:
        """Check for version requirement consistency"""
        issues = []
        
        # Track version requirements for each package
        version_requirements = {}
        
        for cmake_file in self.cmake_files:
            content = cmake_file.read_text()
            
            # Find version requirements
            version_matches = re.findall(
                r'find_package\s*\(\s*(\w+)\s+(?:REQUIRED\s+)?(?:VERSION\s+)?(\d+(?:\.\d+)*)',
                content, re.IGNORECASE
            )
            
            for package, version in version_matches:
                if package not in version_requirements:
                    version_requirements[package] = {}
                version_requirements[package][cmake_file] = version
        
        # Check for inconsistent versions
        for package, file_versions in version_requirements.items():
            versions = set(file_versions.values())
            if len(versions) > 1:
                for file_path, version in file_versions.items():
                    issues.append(CMakeIssue(
                        file_path=file_path,
                        line_number=1,
                        issue_type=CMakeIssueType.VERSION_MISMATCH,
                        description=f"Inconsistent version requirement for {package}: {version}",
                        severity="error",
                        suggested_fix=f"Use consistent version across all files: {max(versions)}"
                    ))
        
        return issues


class CMakeFixer:
    """Fixes CMake issues automatically"""
    
    def __init__(self, project_root: Path):
        self.project_root = project_root
        self.logger = logging.getLogger(self.__class__.__name__)
    
    def fix_issue(self, issue: CMakeIssue, dry_run: bool = False) -> bool:
        """Fix a single CMake issue"""
        
        if issue.issue_type == CMakeIssueType.DEPRECATED_COMMAND:
            return self._fix_deprecated_command(issue, dry_run)
        elif issue.issue_type == CMakeIssueType.INCORRECT_LINK_LIBRARIES:
            return self._fix_link_libraries(issue, dry_run)
        elif issue.issue_type == CMakeIssueType.VERSION_MISMATCH:
            return self._fix_version_mismatch(issue, dry_run)
        elif issue.issue_type == CMakeIssueType.TEST_CONFIGURATION:
            return self._fix_test_configuration(issue, dry_run)
        else:
            self.logger.warning(f"No automatic fix available for {issue.issue_type}")
            return False
    
    def _fix_deprecated_command(self, issue: CMakeIssue, dry_run: bool) -> bool:
        """Fix deprecated CMake commands"""
        try:
            content = issue.file_path.read_text()
            lines = content.split('\n')
            
            if issue.line_number <= len(lines):
                old_line = lines[issue.line_number - 1]
                new_line = self._modernize_cmake_command(old_line)
                
                if old_line != new_line:
                    lines[issue.line_number - 1] = new_line
                    
                    if not dry_run:
                        issue.file_path.write_text('\n'.join(lines))
                        self.logger.info(f"Fixed deprecated command in {issue.file_path}:{issue.line_number}")
                    else:
                        self.logger.info(f"[DRY RUN] Would fix: {old_line} -> {new_line}")
                    
                    return True
            
        except Exception as e:
            self.logger.error(f"Error fixing issue: {e}")
        
        return False
    
    def _modernize_cmake_command(self, line: str) -> str:
        """Modernize a CMake command"""
        # Replace include_directories with target_include_directories
        if "include_directories" in line and "target_include_directories" not in line:
            # This is a simplified replacement - real implementation would be more sophisticated
            match = re.match(r'(\s*)include_directories\s*\((.*)\)', line)
            if match:
                indent, dirs = match.groups()
                # Assume we're dealing with a target named ${PROJECT_NAME}
                return f"{indent}target_include_directories(${{PROJECT_NAME}} PUBLIC {dirs})"
        
        # Replace add_definitions with target_compile_definitions
        if "add_definitions" in line:
            match = re.match(r'(\s*)add_definitions\s*\((.*)\)', line)
            if match:
                indent, defs = match.groups()
                # Remove -D prefix if present
                defs = re.sub(r'-D(\w+)', r'\1', defs)
                return f"{indent}target_compile_definitions(${{PROJECT_NAME}} PUBLIC {defs})"
        
        return line
    
    def _fix_link_libraries(self, issue: CMakeIssue, dry_run: bool) -> bool:
        """Fix target_link_libraries without visibility keywords"""
        try:
            content = issue.file_path.read_text()
            lines = content.split('\n')
            
            if issue.line_number <= len(lines):
                old_line = lines[issue.line_number - 1]
                
                # Add PUBLIC keyword if missing
                if "target_link_libraries" in old_line and not any(kw in old_line for kw in ["PUBLIC", "PRIVATE", "INTERFACE"]):
                    # Insert PUBLIC after the target name
                    match = re.match(r'(\s*target_link_libraries\s*\(\s*\S+)(.*\))', old_line)
                    if match:
                        new_line = f"{match.group(1)} PUBLIC{match.group(2)}"
                        lines[issue.line_number - 1] = new_line
                        
                        if not dry_run:
                            issue.file_path.write_text('\n'.join(lines))
                            self.logger.info(f"Fixed link libraries in {issue.file_path}:{issue.line_number}")
                        else:
                            self.logger.info(f"[DRY RUN] Would fix: {old_line} -> {new_line}")
                        
                        return True
            
        except Exception as e:
            self.logger.error(f"Error fixing issue: {e}")
        
        return False
    
    def _fix_version_mismatch(self, issue: CMakeIssue, dry_run: bool) -> bool:
        """Fix version mismatch issues"""
        # This would require more context to fix properly
        self.logger.info(f"Version mismatch in {issue.file_path} requires manual review")
        return False
    
    def _fix_test_configuration(self, issue: CMakeIssue, dry_run: bool) -> bool:
        """Fix test configuration issues"""
        try:
            content = issue.file_path.read_text()
            lines = content.split('\n')
            
            if issue.line_number <= len(lines):
                # Check if this is an add_test line
                if "add_test" in lines[issue.line_number - 1]:
                    # Look for the test name
                    match = re.match(r'\s*add_test\s*\(\s*NAME\s+(\S+)|add_test\s*\(\s*(\S+)', 
                                   lines[issue.line_number - 1])
                    if match:
                        test_name = match.group(1) or match.group(2)
                        
                        # Add set_tests_properties after add_test
                        properties_line = f"set_tests_properties({test_name} PROPERTIES WORKING_DIRECTORY ${{CMAKE_CURRENT_BINARY_DIR}})"
                        
                        # Check if properties are already set
                        already_has_properties = False
                        for i in range(issue.line_number, min(issue.line_number + 5, len(lines))):
                            if f"set_tests_properties({test_name}" in lines[i]:
                                already_has_properties = True
                                break
                        
                        if not already_has_properties:
                            # Insert the properties line
                            lines.insert(issue.line_number, properties_line)
                            
                            if not dry_run:
                                issue.file_path.write_text('\n'.join(lines))
                                self.logger.info(f"Added test properties in {issue.file_path}")
                            else:
                                self.logger.info(f"[DRY RUN] Would add: {properties_line}")
                            
                            return True
            
        except Exception as e:
            self.logger.error(f"Error fixing test configuration: {e}")
        
        return False


class CMakeBuildTester:
    """Tests CMake builds to verify fixes"""
    
    def __init__(self, project_root: Path):
        self.project_root = project_root
        self.build_dir = project_root / "build"
        self.logger = logging.getLogger(self.__class__.__name__)
    
    def test_configuration(self) -> Tuple[bool, str]:
        """Test CMake configuration"""
        try:
            # Create build directory if it doesn't exist
            self.build_dir.mkdir(exist_ok=True)
            
            # Run cmake
            result = subprocess.run(
                ["cmake", ".."],
                cwd=self.build_dir,
                capture_output=True,
                text=True
            )
            
            success = result.returncode == 0
            output = result.stdout + result.stderr
            
            if not success:
                self.logger.error("CMake configuration failed")
                self.logger.error(output)
            else:
                self.logger.info("CMake configuration successful")
            
            return success, output
            
        except Exception as e:
            return False, str(e)
    
    def test_build(self, target: Optional[str] = None) -> Tuple[bool, str]:
        """Test building with CMake"""
        try:
            cmd = ["cmake", "--build", "."]
            if target:
                cmd.extend(["--target", target])
            
            result = subprocess.run(
                cmd,
                cwd=self.build_dir,
                capture_output=True,
                text=True
            )
            
            success = result.returncode == 0
            output = result.stdout + result.stderr
            
            if not success:
                self.logger.error(f"Build failed for target: {target or 'all'}")
            else:
                self.logger.info(f"Build successful for target: {target or 'all'}")
            
            return success, output
            
        except Exception as e:
            return False, str(e)


def main():
    """Main entry point"""
    parser = argparse.ArgumentParser(
        description="OpenCog CMake Build System Fixer",
        epilog="""
Examples:
  # Analyze all CMake files
  python fix-cmake.py --analyze
  
  # Fix all issues automatically
  python fix-cmake.py --fix
  
  # Dry run to see what would be fixed
  python fix-cmake.py --fix --dry-run
  
  # Test CMake configuration after fixes
  python fix-cmake.py --test
        """
    )
    
    parser.add_argument("--analyze", action="store_true", help="Analyze CMake files for issues")
    parser.add_argument("--fix", action="store_true", help="Fix detected issues")
    parser.add_argument("--dry-run", action="store_true", help="Show what would be fixed without making changes")
    parser.add_argument("--test", action="store_true", help="Test CMake configuration")
    parser.add_argument("--project-root", type=Path, default=Path.cwd(), help="Project root directory")
    
    args = parser.parse_args()
    
    # Setup logging
    logging.basicConfig(
        level=logging.INFO,
        format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
    )
    
    # Create components
    analyzer = CMakeAnalyzer(args.project_root)
    fixer = CMakeFixer(args.project_root)
    tester = CMakeBuildTester(args.project_root)
    
    if args.analyze or args.fix:
        # Analyze CMake files
        issues = analyzer.analyze_all()
        
        print(f"\nFound {len(issues)} CMake issues:\n")
        
        # Group by severity
        by_severity = {"error": [], "warning": [], "info": []}
        for issue in issues:
            by_severity[issue.severity].append(issue)
        
        for severity in ["error", "warning", "info"]:
            if by_severity[severity]:
                print(f"\n{severity.upper()}S ({len(by_severity[severity])}):")
                for issue in by_severity[severity][:10]:  # Show first 10
                    print(f"  {issue.file_path}:{issue.line_number} - {issue.description}")
                    if issue.suggested_fix:
                        print(f"    Fix: {issue.suggested_fix}")
                
                if len(by_severity[severity]) > 10:
                    print(f"  ... and {len(by_severity[severity]) - 10} more")
        
        if args.fix:
            print(f"\n{'='*60}")
            print("Fixing issues...")
            
            fixed_count = 0
            for issue in issues:
                if fixer.fix_issue(issue, dry_run=args.dry_run):
                    fixed_count += 1
            
            print(f"\nFixed {fixed_count} out of {len(issues)} issues")
    
    if args.test:
        print(f"\n{'='*60}")
        print("Testing CMake configuration...")
        
        success, output = tester.test_configuration()
        if success:
            print("✓ CMake configuration successful")
            
            # Try a test build
            print("\nTesting build...")
            success, output = tester.test_build()
            if success:
                print("✓ Build successful")
            else:
                print("✗ Build failed")
                print("See build/CMakeFiles/CMakeError.log for details")
        else:
            print("✗ CMake configuration failed")
            print("Error output:")
            print(output[:1000])  # First 1000 chars


if __name__ == "__main__":
    main() 