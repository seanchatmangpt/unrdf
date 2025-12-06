/**
 * LaTeX Template Pack
 * Professional LaTeX document templates for academic and technical writing
 */

export const templates = {
  'academic-paper': {
    name: 'Academic Paper',
    description: 'IEEE/ACM style academic paper template',
    path: 'latex/academic-paper.njk',
    variables: {
      title: 'Paper title',
      authors: 'Array of author objects with name, affiliation',
      abstract: 'Paper abstract',
      keywords: 'Array of keywords',
      sections: 'Paper sections with content',
      references: 'Bibliography references',
      documentClass: 'LaTeX document class (default: article)'
    }
  },

  'technical-report': {
    name: 'Technical Report',
    description: 'Professional technical report template',
    path: 'latex/technical-report.njk',
    variables: {
      title: 'Report title',
      author: 'Report author',
      organization: 'Organization name',
      date: 'Report date',
      reportNumber: 'Report number/identifier',
      executiveSummary: 'Executive summary',
      sections: 'Report sections',
      appendices: 'Report appendices'
    }
  },

  'thesis': {
    name: 'Thesis/Dissertation',
    description: 'University thesis/dissertation template',
    path: 'latex/thesis.njk',
    variables: {
      title: 'Thesis title',
      author: 'Author name',
      degree: 'Degree type (PhD, Masters)',
      department: 'Department name',
      university: 'University name',
      year: 'Graduation year',
      advisor: 'Thesis advisor',
      committee: 'Committee members',
      chapters: 'Thesis chapters'
    }
  },

  'presentation': {
    name: 'Beamer Presentation',
    description: 'LaTeX Beamer presentation template',
    path: 'latex/presentation.njk',
    variables: {
      title: 'Presentation title',
      author: 'Presenter name',
      institute: 'Institution',
      date: 'Presentation date',
      theme: 'Beamer theme (default: default)',
      slides: 'Array of slide configurations'
    }
  },

  'letter': {
    name: 'Formal Letter',
    description: 'Professional letter template',
    path: 'latex/letter.njk',
    variables: {
      senderName: 'Sender name',
      senderAddress: 'Sender address',
      recipientName: 'Recipient name', 
      recipientAddress: 'Recipient address',
      date: 'Letter date',
      subject: 'Letter subject',
      content: 'Letter content',
      closing: 'Letter closing (default: Sincerely)'
    }
  }
};

export const categories = {
  academic: ['academic-paper', 'thesis'],
  professional: ['technical-report', 'letter'],
  presentation: ['presentation']
};

export const documentClasses = {
  article: 'Standard article format',
  report: 'Report with chapters',
  book: 'Book format with parts and chapters',
  beamer: 'Presentation slides',
  letter: 'Letter format'
};

export const getTemplate = (name) => templates[name];
export const getTemplatesByCategory = (category) => 
  categories[category]?.map(name => templates[name]) || [];

export default templates;