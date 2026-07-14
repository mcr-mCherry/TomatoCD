document.addEventListener('DOMContentLoaded', function() {
  const canvas = document.getElementById('tomato-particles');
  if (!canvas) return;
  
  const ctx = canvas.getContext('2d');
  const infoSection = document.querySelector('.info-section');
  
  function resizeCanvas() {
    if (infoSection) {
      canvas.width = infoSection.offsetWidth;
      canvas.height = infoSection.offsetHeight;
    }
  }
  
  resizeCanvas();
  window.addEventListener('resize', resizeCanvas);
  
  const particles = [];
  const particleCount = 150;
  const mouse = { x: canvas.width / 2, y: canvas.height / 2 };
  
  for (let i = 0; i < particleCount; i++) {
    const r = Math.floor(Math.random() * 100 + 155);
    const g = Math.floor(Math.random() * 100 + 50);
    const b = Math.floor(Math.random() * 50 + 50);
    const a = (Math.random() * 0.5 + 0.3).toFixed(2);
    particles.push({
      x: Math.random() * canvas.width,
      y: Math.random() * canvas.height,
      size: Math.random() * 3 + 1,
      speedX: Math.random() * 0.5 - 0.25,
      speedY: Math.random() * 0.5 - 0.25,
      color: 'rgba(' + r + ',' + g + ',' + b + ',' + a + ')'
    });
  }
  
  function isInTomatoShape(x, y) {
    const centerX = canvas.width / 2;
    const centerY = canvas.height / 2;
    const scale = Math.min(canvas.width, canvas.height) / 300;
    
    const normalizedX = (x - centerX) / scale;
    const normalizedY = (y - centerY) / scale;
    
    const radius = 100;
    const height = 150;
    
    if (normalizedY < -height / 2) return false;
    if (normalizedY > height / 2) return false;
    
    const yFactor = 1 - Math.abs(normalizedY) / (height / 2);
    const currentRadius = radius * yFactor;
    
    const distance = Math.sqrt(normalizedX * normalizedX + normalizedY * normalizedY * 0.5);
    return distance <= currentRadius;
  }
  
  function animate() {
    ctx.clearRect(0, 0, canvas.width, canvas.height);
    
    particles.forEach(particle => {
      const dx = mouse.x - particle.x;
      const dy = mouse.y - particle.y;
      const distance = Math.sqrt(dx * dx + dy * dy);
      const maxDistance = 150;
      
      if (distance < maxDistance) {
        const force = (maxDistance - distance) / maxDistance;
        particle.x += dx * force * 0.02;
        particle.y += dy * force * 0.02;
      }
      
      if (isInTomatoShape(particle.x, particle.y)) {
        particle.x += particle.speedX;
        particle.y += particle.speedY;
      } else {
        const centerX = canvas.width / 2;
        const centerY = canvas.height / 2;
        const angle = Math.atan2(centerY - particle.y, centerX - particle.x);
        particle.x += Math.cos(angle) * 2;
        particle.y += Math.sin(angle) * 2;
      }
      
      if (particle.x < 0) particle.x = canvas.width;
      if (particle.x > canvas.width) particle.x = 0;
      if (particle.y < 0) particle.y = canvas.height;
      if (particle.y > canvas.height) particle.y = 0;
      
      ctx.beginPath();
      ctx.arc(particle.x, particle.y, particle.size, 0, Math.PI * 2);
      ctx.fillStyle = particle.color;
      ctx.fill();
    });
    
    requestAnimationFrame(animate);
  }
  
  infoSection.addEventListener('mousemove', function(e) {
    const rect = infoSection.getBoundingClientRect();
    mouse.x = e.clientX - rect.left;
    mouse.y = e.clientY - rect.top;
  });
  
  infoSection.addEventListener('mouseleave', function() {
    mouse.x = canvas.width / 2;
    mouse.y = canvas.height / 2;
  });
  
  animate();
});