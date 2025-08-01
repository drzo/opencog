import { useState, useMemo, useEffect } from 'react'
import { Button } from '@/components/ui/button.jsx'
import { Input } from '@/components/ui/input.jsx'
import { Badge } from '@/components/ui/badge.jsx'
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card.jsx'
import { Tabs, TabsContent, TabsList, TabsTrigger } from '@/components/ui/tabs.jsx'
import { Select, SelectContent, SelectItem, SelectTrigger, SelectValue } from '@/components/ui/select.jsx'
import { ChevronUp, ChevronDown, Search, Filter, BarChart3, Brain, Cpu, Shield, Zap, GitBranch } from 'lucide-react'
import { BarChart, Bar, XAxis, YAxis, CartesianGrid, Tooltip, ResponsiveContainer, RadarChart, PolarGrid, PolarAngleAxis, PolarRadiusAxis, Radar, Legend } from 'recharts'
import comparisonData from './assets/comparison_matrix_data.json'
import './App.css'

function App() {
  const [sortField, setSortField] = useState('name')
  const [sortDirection, setSortDirection] = useState('asc')
  const [filterCategory, setFilterCategory] = useState('all')
  const [filterType, setFilterType] = useState('all')
  const [searchTerm, setSearchTerm] = useState('')
  const [selectedPackage, setSelectedPackage] = useState(null)
  const [activeTab, setActiveTab] = useState('matrix')

  const dimensions = comparisonData.dimensions
  const packages = comparisonData.packages
  const hurdIssues = comparisonData.hurdIssues

  // Filter and sort packages
  const filteredAndSortedPackages = useMemo(() => {
    let filtered = packages.filter(pkg => {
      const matchesSearch = pkg.name.toLowerCase().includes(searchTerm.toLowerCase()) ||
                           pkg.description.toLowerCase().includes(searchTerm.toLowerCase())
      const matchesCategory = filterCategory === 'all' || pkg.category === filterCategory
      const matchesType = filterType === 'all' || pkg.type === filterType
      return matchesSearch && matchesCategory && matchesType
    })

    return filtered.sort((a, b) => {
      let aVal = a[sortField]
      let bVal = b[sortField]
      
      if (sortField.startsWith('scores.')) {
        const scoreField = sortField.replace('scores.', '')
        aVal = a.scores[scoreField]
        bVal = b.scores[scoreField]
      }
      
      if (typeof aVal === 'string') {
        aVal = aVal.toLowerCase()
        bVal = bVal.toLowerCase()
      }
      
      if (sortDirection === 'asc') {
        return aVal < bVal ? -1 : aVal > bVal ? 1 : 0
      } else {
        return aVal > bVal ? -1 : aVal < bVal ? 1 : 0
      }
    })
  }, [packages, sortField, sortDirection, filterCategory, filterType, searchTerm])

  const handleSort = (field) => {
    if (sortField === field) {
      setSortDirection(sortDirection === 'asc' ? 'desc' : 'asc')
    } else {
      setSortField(field)
      setSortDirection('asc')
    }
  }

  const getScoreColor = (score) => {
    if (score >= 8) return 'bg-green-500'
    if (score >= 6) return 'bg-yellow-500'
    if (score >= 4) return 'bg-orange-500'
    return 'bg-red-500'
  }

  const getTypeIcon = (type) => {
    return type === 'opencog' ? <Brain className="w-4 h-4" /> : <Cpu className="w-4 h-4" />
  }

  const getTypeColor = (type) => {
    return type === 'opencog' ? 'bg-purple-100 text-purple-800' : 'bg-blue-100 text-blue-800'
  }

  // Prepare chart data
  const chartData = useMemo(() => {
    return filteredAndSortedPackages.map(pkg => ({
      name: pkg.name.split(' ').slice(-1)[0], // Use last word for shorter labels
      ...pkg.scores
    }))
  }, [filteredAndSortedPackages])

  const radarData = useMemo(() => {
    if (!selectedPackage) return []
    
    return Object.entries(selectedPackage.scores).map(([key, value]) => ({
      dimension: key.replace(/([A-Z])/g, ' $1').replace(/^./, str => str.toUpperCase()),
      value: value,
      fullMark: 10
    }))
  }, [selectedPackage])

  const SortButton = ({ field, children }) => (
    <Button
      variant="ghost"
      size="sm"
      onClick={() => handleSort(field)}
      className="h-8 px-2 text-xs font-medium"
    >
      {children}
      {sortField === field && (
        sortDirection === 'asc' ? <ChevronUp className="w-3 h-3 ml-1" /> : <ChevronDown className="w-3 h-3 ml-1" />
      )}
    </Button>
  )

  return (
    <div className="min-h-screen bg-gradient-to-br from-slate-50 to-slate-100 dark:from-slate-900 dark:to-slate-800">
      <div className="container mx-auto px-4 py-8">
        {/* Header */}
        <div className="text-center mb-8">
          <h1 className="text-4xl font-bold text-slate-900 dark:text-slate-100 mb-4">
            OpenCog vs GNU Packages
          </h1>
          <p className="text-xl text-slate-600 dark:text-slate-400 mb-2">
            Comprehensive Comparison Matrix
          </p>
          <p className="text-sm text-slate-500 dark:text-slate-500">
            Multi-dimensional analysis in the context of GNU Hurd microkernel issues
          </p>
          <div className="flex justify-center gap-4 mt-4">
            <Badge variant="outline" className="flex items-center gap-1">
              <Brain className="w-3 h-3" />
              {packages.filter(p => p.type === 'opencog').length} OpenCog Subsystems
            </Badge>
            <Badge variant="outline" className="flex items-center gap-1">
              <Cpu className="w-3 h-3" />
              {packages.filter(p => p.type === 'gnu').length} GNU Packages
            </Badge>
            <Badge variant="outline" className="flex items-center gap-1">
              <Shield className="w-3 h-3" />
              {hurdIssues.length} Root Causes Analyzed
            </Badge>
          </div>
        </div>

        {/* Tabs */}
        <Tabs value={activeTab} onValueChange={setActiveTab} className="w-full">
          <TabsList className="grid w-full grid-cols-4">
            <TabsTrigger value="matrix">Comparison Matrix</TabsTrigger>
            <TabsTrigger value="charts">Score Charts</TabsTrigger>
            <TabsTrigger value="details">Package Details</TabsTrigger>
            <TabsTrigger value="hurd-issues">Hurd Issues</TabsTrigger>
          </TabsList>

          {/* Comparison Matrix Tab */}
          <TabsContent value="matrix" className="space-y-6">
            {/* Filters */}
            <Card>
              <CardHeader>
                <CardTitle className="flex items-center gap-2">
                  <Filter className="w-5 h-5" />
                  Filters & Search
                </CardTitle>
              </CardHeader>
              <CardContent>
                <div className="grid grid-cols-1 md:grid-cols-4 gap-4">
                  <div className="relative">
                    <Search className="absolute left-3 top-3 h-4 w-4 text-slate-400" />
                    <Input
                      placeholder="Search packages..."
                      value={searchTerm}
                      onChange={(e) => setSearchTerm(e.target.value)}
                      className="pl-10"
                    />
                  </div>
                  <Select value={filterCategory} onValueChange={setFilterCategory}>
                    <SelectTrigger>
                      <SelectValue placeholder="Filter by category" />
                    </SelectTrigger>
                    <SelectContent>
                      <SelectItem value="all">All Categories</SelectItem>
                      <SelectItem value="OpenCog Subsystems">OpenCog Subsystems</SelectItem>
                      <SelectItem value="GNU Base System">GNU Base System</SelectItem>
                      <SelectItem value="GNU Development Tools">GNU Development Tools</SelectItem>
                      <SelectItem value="GNU User Applications">GNU User Applications</SelectItem>
                    </SelectContent>
                  </Select>
                  <Select value={filterType} onValueChange={setFilterType}>
                    <SelectTrigger>
                      <SelectValue placeholder="Filter by type" />
                    </SelectTrigger>
                    <SelectContent>
                      <SelectItem value="all">All Types</SelectItem>
                      <SelectItem value="opencog">OpenCog</SelectItem>
                      <SelectItem value="gnu">GNU</SelectItem>
                    </SelectContent>
                  </Select>
                  <div className="text-sm text-slate-600 dark:text-slate-400 flex items-center">
                    Showing {filteredAndSortedPackages.length} of {packages.length} packages
                  </div>
                </div>
              </CardContent>
            </Card>

            {/* Comparison Table */}
            <Card>
              <CardHeader>
                <CardTitle>Package Comparison Matrix</CardTitle>
                <CardDescription>
                  Click column headers to sort. Scores are rated 1-10 (higher is better).
                </CardDescription>
              </CardHeader>
              <CardContent>
                <div className="overflow-x-auto">
                  <table className="w-full text-sm">
                    <thead>
                      <tr className="border-b">
                        <th className="text-left p-2">
                          <SortButton field="name">Package</SortButton>
                        </th>
                        <th className="text-left p-2">
                          <SortButton field="type">Type</SortButton>
                        </th>
                        <th className="text-left p-2">
                          <SortButton field="scores.coreFeatures">Core Features</SortButton>
                        </th>
                        <th className="text-left p-2">
                          <SortButton field="scores.architectureModel">Architecture</SortButton>
                        </th>
                        <th className="text-left p-2">
                          <SortButton field="scores.designEffectiveness">Design Effectiveness</SortButton>
                        </th>
                        <th className="text-left p-2">
                          <SortButton field="scores.hurdIssueRelevance">Hurd Relevance</SortButton>
                        </th>
                        <th className="text-left p-2">
                          <SortButton field="scores.cognitiveCapabilities">Cognitive</SortButton>
                        </th>
                        <th className="text-left p-2">
                          <SortButton field="scores.packageReliability">Reliability</SortButton>
                        </th>
                        <th className="text-left p-2">Actions</th>
                      </tr>
                    </thead>
                    <tbody>
                      {filteredAndSortedPackages.map((pkg) => (
                        <tr key={pkg.id} className="border-b hover:bg-slate-50 dark:hover:bg-slate-800">
                          <td className="p-2">
                            <div className="flex items-center gap-2">
                              {getTypeIcon(pkg.type)}
                              <div>
                                <div className="font-medium">{pkg.name}</div>
                                <div className="text-xs text-slate-500">{pkg.description.substring(0, 50)}...</div>
                              </div>
                            </div>
                          </td>
                          <td className="p-2">
                            <Badge className={getTypeColor(pkg.type)}>
                              {pkg.type === 'opencog' ? 'OpenCog' : 'GNU'}
                            </Badge>
                          </td>
                          <td className="p-2">
                            <div className="flex items-center gap-2">
                              <div className={`w-6 h-6 rounded ${getScoreColor(pkg.scores.coreFeatures)} flex items-center justify-center text-white text-xs font-bold`}>
                                {pkg.scores.coreFeatures}
                              </div>
                            </div>
                          </td>
                          <td className="p-2">
                            <div className="flex items-center gap-2">
                              <div className={`w-6 h-6 rounded ${getScoreColor(pkg.scores.architectureModel)} flex items-center justify-center text-white text-xs font-bold`}>
                                {pkg.scores.architectureModel}
                              </div>
                            </div>
                          </td>
                          <td className="p-2">
                            <div className="flex items-center gap-2">
                              <div className={`w-6 h-6 rounded ${getScoreColor(pkg.scores.designEffectiveness)} flex items-center justify-center text-white text-xs font-bold`}>
                                {pkg.scores.designEffectiveness}
                              </div>
                            </div>
                          </td>
                          <td className="p-2">
                            <div className="flex items-center gap-2">
                              <div className={`w-6 h-6 rounded ${getScoreColor(pkg.scores.hurdIssueRelevance)} flex items-center justify-center text-white text-xs font-bold`}>
                                {pkg.scores.hurdIssueRelevance}
                              </div>
                            </div>
                          </td>
                          <td className="p-2">
                            <div className="flex items-center gap-2">
                              <div className={`w-6 h-6 rounded ${getScoreColor(pkg.scores.cognitiveCapabilities)} flex items-center justify-center text-white text-xs font-bold`}>
                                {pkg.scores.cognitiveCapabilities}
                              </div>
                            </div>
                          </td>
                          <td className="p-2">
                            <div className="flex items-center gap-2">
                              <div className={`w-6 h-6 rounded ${getScoreColor(pkg.scores.packageReliability)} flex items-center justify-center text-white text-xs font-bold`}>
                                {pkg.scores.packageReliability}
                              </div>
                            </div>
                          </td>
                          <td className="p-2">
                            <Button
                              size="sm"
                              variant="outline"
                              onClick={() => {
                                setSelectedPackage(pkg)
                                setActiveTab('details')
                              }}
                            >
                              Details
                            </Button>
                          </td>
                        </tr>
                      ))}
                    </tbody>
                  </table>
                </div>
              </CardContent>
            </Card>
          </TabsContent>

          {/* Charts Tab */}
          <TabsContent value="charts" className="space-y-6">
            <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
              <Card>
                <CardHeader>
                  <CardTitle>Core Features Comparison</CardTitle>
                </CardHeader>
                <CardContent>
                  <ResponsiveContainer width="100%" height={300}>
                    <BarChart data={chartData}>
                      <CartesianGrid strokeDasharray="3 3" />
                      <XAxis dataKey="name" />
                      <YAxis domain={[0, 10]} />
                      <Tooltip />
                      <Bar dataKey="coreFeatures" fill="#8884d8" />
                    </BarChart>
                  </ResponsiveContainer>
                </CardContent>
              </Card>

              <Card>
                <CardHeader>
                  <CardTitle>Hurd Issue Relevance</CardTitle>
                </CardHeader>
                <CardContent>
                  <ResponsiveContainer width="100%" height={300}>
                    <BarChart data={chartData}>
                      <CartesianGrid strokeDasharray="3 3" />
                      <XAxis dataKey="name" />
                      <YAxis domain={[0, 10]} />
                      <Tooltip />
                      <Bar dataKey="hurdIssueRelevance" fill="#82ca9d" />
                    </BarChart>
                  </ResponsiveContainer>
                </CardContent>
              </Card>

              <Card>
                <CardHeader>
                  <CardTitle>Cognitive Capabilities</CardTitle>
                </CardHeader>
                <CardContent>
                  <ResponsiveContainer width="100%" height={300}>
                    <BarChart data={chartData}>
                      <CartesianGrid strokeDasharray="3 3" />
                      <XAxis dataKey="name" />
                      <YAxis domain={[0, 10]} />
                      <Tooltip />
                      <Bar dataKey="cognitiveCapabilities" fill="#ffc658" />
                    </BarChart>
                  </ResponsiveContainer>
                </CardContent>
              </Card>

              <Card>
                <CardHeader>
                  <CardTitle>Overall Architecture Comparison</CardTitle>
                </CardHeader>
                <CardContent>
                  <ResponsiveContainer width="100%" height={300}>
                    <BarChart data={chartData}>
                      <CartesianGrid strokeDasharray="3 3" />
                      <XAxis dataKey="name" />
                      <YAxis domain={[0, 10]} />
                      <Tooltip />
                      <Bar dataKey="architectureModel" fill="#ff7300" />
                    </BarChart>
                  </ResponsiveContainer>
                </CardContent>
              </Card>
            </div>
          </TabsContent>

          {/* Package Details Tab */}
          <TabsContent value="details" className="space-y-6">
            {selectedPackage ? (
              <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
                <Card>
                  <CardHeader>
                    <CardTitle className="flex items-center gap-2">
                      {getTypeIcon(selectedPackage.type)}
                      {selectedPackage.name}
                    </CardTitle>
                    <CardDescription>{selectedPackage.description}</CardDescription>
                  </CardHeader>
                  <CardContent className="space-y-4">
                    <div className="grid grid-cols-2 gap-4 text-sm">
                      <div>
                        <span className="font-medium">Category:</span> {selectedPackage.category}
                      </div>
                      <div>
                        <span className="font-medium">Language:</span> {selectedPackage.language}
                      </div>
                      <div>
                        <span className="font-medium">Last Release:</span> {selectedPackage.lastRelease}
                      </div>
                      <div>
                        <span className="font-medium">Contributors:</span> {selectedPackage.contributors}
                      </div>
                      <div>
                        <span className="font-medium">Lines of Code:</span> {selectedPackage.linesOfCode.toLocaleString()}
                      </div>
                      <div>
                        <span className="font-medium">Stars:</span> {selectedPackage.stars}
                      </div>
                    </div>
                    
                    <div>
                      <span className="font-medium">Dependencies:</span>
                      <div className="flex flex-wrap gap-1 mt-1">
                        {selectedPackage.dependencies.map((dep, idx) => (
                          <Badge key={idx} variant="secondary" className="text-xs">{dep}</Badge>
                        ))}
                      </div>
                    </div>

                    <div>
                      <span className="font-medium">Cognitive Features:</span>
                      <div className="flex flex-wrap gap-1 mt-1">
                        {selectedPackage.cognitiveFeatures.map((feature, idx) => (
                          <Badge key={idx} variant="outline" className="text-xs">{feature}</Badge>
                        ))}
                      </div>
                    </div>

                    <div>
                      <a 
                        href={selectedPackage.repository} 
                        target="_blank" 
                        rel="noopener noreferrer"
                        className="inline-flex items-center gap-1 text-blue-600 hover:text-blue-800"
                      >
                        <GitBranch className="w-4 h-4" />
                        View Repository
                      </a>
                    </div>
                  </CardContent>
                </Card>

                <Card>
                  <CardHeader>
                    <CardTitle>Score Radar Chart</CardTitle>
                  </CardHeader>
                  <CardContent>
                    <ResponsiveContainer width="100%" height={300}>
                      <RadarChart data={radarData}>
                        <PolarGrid />
                        <PolarAngleAxis dataKey="dimension" />
                        <PolarRadiusAxis angle={90} domain={[0, 10]} />
                        <Radar
                          name={selectedPackage.name}
                          dataKey="value"
                          stroke="#8884d8"
                          fill="#8884d8"
                          fillOpacity={0.3}
                        />
                      </RadarChart>
                    </ResponsiveContainer>
                  </CardContent>
                </Card>

                <Card className="lg:col-span-2">
                  <CardHeader>
                    <CardTitle>Solutions to GNU Hurd Issues</CardTitle>
                  </CardHeader>
                  <CardContent>
                    <div className="space-y-4">
                      {Object.entries(selectedPackage.hurdSolutions).map(([issueId, solution]) => {
                        const issue = hurdIssues.find(h => h.id === issueId)
                        return (
                          <div key={issueId} className="border rounded-lg p-4">
                            <div className="flex items-center gap-2 mb-2">
                              <Badge variant={issue.severity === 'Critical' ? 'destructive' : issue.severity === 'High' ? 'default' : 'secondary'}>
                                {issue.severity}
                              </Badge>
                              <span className="font-medium">{issue.name}</span>
                              <span className="text-sm text-slate-500">({issue.affectedIssues} issues)</span>
                            </div>
                            <p className="text-sm text-slate-600 dark:text-slate-400 mb-2">{issue.description}</p>
                            <p className="text-sm">{solution}</p>
                          </div>
                        )
                      })}
                    </div>
                  </CardContent>
                </Card>
              </div>
            ) : (
              <Card>
                <CardContent className="text-center py-12">
                  <p className="text-slate-500">Select a package from the comparison matrix to view detailed information.</p>
                </CardContent>
              </Card>
            )}
          </TabsContent>

          {/* Hurd Issues Tab */}
          <TabsContent value="hurd-issues" className="space-y-6">
            <Card>
              <CardHeader>
                <CardTitle>GNU Hurd Root Cause Issues</CardTitle>
                <CardDescription>
                  Five fundamental architectural gaps that manifest as 350+ individual issues
                </CardDescription>
              </CardHeader>
              <CardContent>
                <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4">
                  {hurdIssues.map((issue) => (
                    <Card key={issue.id} className="border-l-4 border-l-red-500">
                      <CardHeader>
                        <div className="flex items-center justify-between">
                          <CardTitle className="text-lg">{issue.name}</CardTitle>
                          <Badge variant={issue.severity === 'Critical' ? 'destructive' : issue.severity === 'High' ? 'default' : 'secondary'}>
                            {issue.severity}
                          </Badge>
                        </div>
                      </CardHeader>
                      <CardContent>
                        <p className="text-sm text-slate-600 dark:text-slate-400 mb-3">{issue.description}</p>
                        <div className="flex items-center gap-2">
                          <Zap className="w-4 h-4 text-red-500" />
                          <span className="text-sm font-medium">{issue.affectedIssues} affected issues</span>
                        </div>
                      </CardContent>
                    </Card>
                  ))}
                </div>
              </CardContent>
            </Card>

            <Card>
              <CardHeader>
                <CardTitle>Issue Resolution Potential</CardTitle>
                <CardDescription>
                  How OpenCog subsystems address each root cause
                </CardDescription>
              </CardHeader>
              <CardContent>
                <div className="space-y-6">
                  {hurdIssues.map((issue) => {
                    const relevantPackages = packages.filter(pkg => 
                      pkg.type === 'opencog' && pkg.scores.hurdIssueRelevance >= 7
                    )
                    
                    return (
                      <div key={issue.id} className="border rounded-lg p-4">
                        <div className="flex items-center gap-2 mb-3">
                          <Badge variant={issue.severity === 'Critical' ? 'destructive' : issue.severity === 'High' ? 'default' : 'secondary'}>
                            {issue.severity}
                          </Badge>
                          <span className="font-medium text-lg">{issue.name}</span>
                        </div>
                        <p className="text-sm text-slate-600 dark:text-slate-400 mb-4">{issue.description}</p>
                        
                        <div className="space-y-2">
                          <span className="font-medium text-sm">OpenCog Solutions:</span>
                          {relevantPackages.map((pkg) => (
                            <div key={pkg.id} className="flex items-start gap-2 text-sm">
                              <Brain className="w-4 h-4 text-purple-600 mt-0.5 flex-shrink-0" />
                              <div>
                                <span className="font-medium">{pkg.name}:</span> {pkg.hurdSolutions[issue.id]}
                              </div>
                            </div>
                          ))}
                        </div>
                      </div>
                    )
                  })}
                </div>
              </CardContent>
            </Card>
          </TabsContent>
        </Tabs>
      </div>
    </div>
  )
}

export default App

