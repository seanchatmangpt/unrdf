/**
 * Microsoft Office Template Pack
 * Templates for generating Office documents (Word, Excel, PowerPoint)
 */

export const templates = {
  'word-document': {
    name: 'Word Document',
    description: 'Microsoft Word document with structured content',
    path: 'office/docx/document.njk',
    variables: {
      title: 'Document title',
      author: 'Document author',
      company: 'Company name',
      sections: 'Document sections (array)',
      includeHeader: 'Include document header (boolean)',
      includeFooter: 'Include document footer (boolean)'
    }
  },

  'word-report': {
    name: 'Word Report',
    description: 'Professional report template for Word',
    path: 'office/docx/report.njk',
    variables: {
      title: 'Report title',
      author: 'Report author',
      date: 'Report date',
      executive_summary: 'Executive summary text',
      sections: 'Report sections with content',
      appendices: 'Report appendices (optional)'
    }
  },

  'excel-workbook': {
    name: 'Excel Workbook',
    description: 'Excel workbook with multiple worksheets',
    path: 'office/xlsx/workbook.njk',
    variables: {
      workbookName: 'Name of the workbook',
      worksheets: 'Array of worksheet configurations',
      includeCharts: 'Include chart definitions (boolean)',
      includePivot: 'Include pivot table definitions (boolean)'
    }
  },

  'powerpoint-presentation': {
    name: 'PowerPoint Presentation',
    description: 'PowerPoint presentation with slides',
    path: 'office/pptx/presentation.njk',
    variables: {
      title: 'Presentation title',
      author: 'Presentation author',
      slides: 'Array of slide configurations',
      theme: 'Presentation theme',
      includeNotes: 'Include speaker notes (boolean)'
    }
  }
};

export const categories = {
  word: ['word-document', 'word-report'],
  excel: ['excel-workbook'],
  powerpoint: ['powerpoint-presentation']
};

export const formats = {
  docx: 'Microsoft Word Document',
  xlsx: 'Microsoft Excel Workbook', 
  pptx: 'Microsoft PowerPoint Presentation'
};

export const getTemplate = (name) => templates[name];
export const getTemplatesByCategory = (category) => 
  categories[category]?.map(name => templates[name]) || [];
export const getTemplatesByFormat = (format) =>
  Object.values(templates).filter(t => t.path.includes(`/${format}/`));

export default templates;